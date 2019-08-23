#' Match points to lines
#' 
match_pt_line <- function(point = NULL, line = NULL, start_buffer = 0, inc_buffer = .1, max_buffer = Inf) {

  stopifnot(!is.null(point) | !is.null(line))
  # Start a list of length of the point list  
  line_id <- integer(nrow(point))
  point$id <- seq(1, nrow(point))
  names(line_id) <- point$id
  line$id <- seq(1, nrow(line))

  counter <- 0
  pt_temp <- point
  buff_size <- start_buffer + inc_buffer * counter

  # while
  while (any(line_id == 0) & buff_size < max_buffer) {
    # Begin with the buffer:
    pt_buffer <- st_buffer(pt_temp, dist = buff_size)

    intersect_line <- st_intersects(pt_buffer, line)
    mask_single <- map_lgl(intersect_line,
      function(x) ifelse(length(x) == 1, TRUE, FALSE))
    #message(paste0(sum(mask_single), " pts have matches with one line."))
    # Fill list with those that have one match
    line_id[names(line_id) %in% pt_temp$id[mask_single]] <-
      sapply(intersect_line[mask_single], function(x) x[1])

    # For multiple matches, get the closer
    mask_multi <- map_lgl(intersect_line,
      function(x) ifelse(length(x) > 1, TRUE, FALSE))
    if (any(mask_multi)) {
      message(paste0(sum(mask_multi), " pts have matches with multi."))
      # Fill list with the closer:
      line_id[names(line_id) %in% pt_temp$id[mask_multi]] <- map2_dbl(which(mask_multi), intersect_line[mask_multi],
	function(pts_id, line_id) {
	  dis <- st_distance(pt_temp[pts_id, ], line[line_id, ])
	  line_id[which.min(dis[1, ])]
	})
    }
    # Increase buffer size
    counter <- counter + 1
    buff_size <- start_buffer + inc_buffer * counter
    pt_temp <- filter(point, id %in% names(line_id)[line_id == 0])
    message(paste0("Buffer size = ", buff_size,". ", nrow(pt_temp)," points do not have lines."))

  }
  line_id
}

#' Interpolate variable by date over stream network
#'
#' The function uses SSN package to perform interpolation through grouping date.
#' It return a data.frame with the id of the station, value interpolation as
#' well of the results of cross-validation.
#'
#' @param ssn a ssn object.
#' @param data data.frame containing the data  
#' @param group the grouping date variable
#' @param var variable to interpolate
#' @param formula formula for the spatial model, see glmssn
#' @param corModel character vector. see corModels argument in glmssn
interpolate_ssn <- function(ssn = NULL, data = NULL, group = NULL, var = NULL,
  formula = NULL, family = NULL, corModel = NULL, pred_name = "station") {

  group <- rlang::enquo(group)
  var <- rlang::enquo(var)
  var_chr <- rlang::quo_name(var)

  # Check:
  stopifnot(var_chr %in% colnames(data))
  stopifnot(pred_name %in% names(names(ssn)))

  #Get data:
  data %<>%
    dplyr::group_by(!!group) %>%
    tidyr::nest()

  # Model, cross-validation and prediction
  data %<>%
    mutate(
      model = purrr::map(data, compute_glmssn, var = var, ssn = ssn, formula =
	formula, family = family, corModel = corModel),
      cross_v = purrr::map(model, function(x) {
	if (!is.na(x)) {
	out <- SSN::CrossValidationStatsSSN(x)
	unlist(out)
	} else {
	  NA
	}
	}),
      prediction = purrr::map(model, function(m){
	if (!is.na(m)) {
	  pred <- predict(m, pred_name)
	  pred$ssn.object@predpoints@SSNPoints[[1]]@point.data[, c("id",var_chr, paste0(var_chr, ".predSE"))]
	} else {
	  NA
	}
	})
    )
  data
}

#' Compute glmss model
#'
#' To feed in interpolate_ssn 
compute_glmssn <- function(data = NULL, var = NULL,
  ssn = NULL, formula = NULL, family = NULL, corModel = NULL, enquo_var = FALSE, ...) {

  if (!is.character(var)) {
    if (enquo_var) {
    var <- rlang::enquo(var)
    }
    var <- rlang::quo_name(var)
  }

  # get ssn dataset to fill it
  data_id_var <- data[, c("id", var)]
  ssn_data <- ssn@obspoints@SSNPoints[[1]]@point.data

  # Suppress var if present
  if (var %in% colnames(ssn_data)) {
    ssn_data <- ssn_data[, ! names(ssn_data) %in% var, drop = F]
  }
  # Fill ssn with new data:
  #stopifnot(nrow(ssn_data) == nrow(data_id_var))
  ssn_data %<>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::left_join(data_id_var, by = "id")
  ssn@obspoints@SSNPoints[[1]]@point.data <- ssn_data

  # temporary hack:
  if (is.null(formula)) {
    formula <-  as.formula(paste0(var," ~ 1"))
  }
  if (is.null(corModel)) {
    corModel <- c("LinearSill.tailup", "Mariah.taildown",
      "Exponential.Euclid")
  }
  if (is.null(family)) {
    family <- "Gaussian"
  }

  mod_sp <- tryCatch(
    {
    model <- SSN::glmssn(formula, ssn, family = family, CorModels = corModel,
      addfunccol = "afv_area")
    return(model)
  },
  error = function (w) {message(w); return(NULL)}
  )
  mod_sp
}

#' Prepare data for interpolation procedure
#'
#' The goal is to prepare the data to be feed by interpolate_ssn  
#' It summarises data by month and year while keeping the ones that have been
#' enough sampled. 
#'
#' @param data a data.frame  
#' @param date date variable 
#' @param var variable to summarise
#' @param donuts data.frame with id of the obs station and coordinates of
#' station location
#' @param id variable containing unique identifier of the stataion
prepare_data_interpolation <- function(data = NULL, date = NULL, var = NULL,
  donuts = NULL, id = NULL, cutoff_day = .5) {

  date <- rlang::enquo(date)
  var <- rlang::enquo(var)
  var_chr <- rlang::quo_name(var)
  id <- rlang::enquo(id)
  id_chr <- rlang::quo_name(id)

  #1. Filter data that are in the obs station
  data %<>%
    dplyr::filter(!!id %in% donuts[[id_chr]])

  #2. Average data by Month:
  data_avg <- data %>%
    dplyr::mutate(
      year = lubridate::year(!!date) %>% as.integer(),
      month = lubridate::month(!!date) %>% as.integer(),
      nb_d_month = lubridate::days_in_month(!!date) %>% as.integer()
      ) %>%
  dplyr::group_by(id, year, month) %>%
  dplyr::summarise(data = mean(!!var, na.rm = TRUE), nobs = sum(!is.na(!!var)),
    pc_obs = nobs / unique(nb_d_month)) #here
  # Filrer month that have enough record
  # + keep year that have one month recorded in each season:
  if (!is.null(cutoff_day)) {
  data_avg_cleaned <- data_avg %>%
    dplyr::mutate(
      data = replace(data, pc_obs < .5, NA)
      )
  } else{
   data_avg_cleaned <- data_avg
  }
  data_avg_cleaned %<>%
    dplyr::mutate(
      year_mon = lubridate::ymd(paste0(year, "-", month, "-", "01")),
      season = lubridate::quarter(year_mon)
      ) %>%
  dplyr::group_by(id, year) %>%
  dplyr::summarise(avg_data = mean(data, na.rm = TRUE),
    is_ok = ifelse(
      sum(!is.na(data)) >= 4 &# at least 4 months of registration
	length(unique(season[!is.na(data)])) == 4# at least 4 season 
      , TRUE, FALSE
    )
  )
  # Keep years that have been enough sampled:
  data_avg_cleaned %<>%
    dplyr::mutate(avg_data = replace(avg_data, is_ok != TRUE, NA))

  # 4. Complete the dataset with missing year:
  #For each id station, replace missing year by NA:
  test <- list(
    year = unique(data_avg_cleaned$year),
    id = unique(as.data.frame(donuts)[, id_chr])
  )
  comb_year_id <- expand.grid(test) %>%
    as_tibble
  data_avg_complete <- comb_year_id %>%
    dplyr::left_join(dplyr::select(data_avg_cleaned, -is_ok))
  if (sum(is.na(data_avg_complete[[var_chr]])) == nrow(data_avg_complete)) {
   message("Only NA in the dataset.")
  }

  data_avg_complete
}

#' Prepare pulse for interpolation procedure
#'
#' The goal is to prepare the data to be feed by interpolate_ssn  
#' It summarises data by month and find if it is a pulse or not 
#'
#' @param data a data.frame  
#' @param date date variable 
#' @param var variable to summarise
#' @param donuts data.frame with id of the obs station and coordinates of
#' station location
#' @param id variable containing unique identifier of the stataion
prepare_pulse_interpolation <- function(data = NULL, date = NULL, var = NULL,
  donuts = NULL, id = NULL, low_threshold = .1, high_threshold = .9) {

  date <- rlang::enquo(date)
  var <- rlang::enquo(var)
  id <- rlang::enquo(id)
  id_chr <- rlang::quo_name(id)

  #1. Filter data that are in the obs station
  data %<>%
    dplyr::filter(!!id %in% donuts[[id_chr]])

  #2. Average data by Month:
  data_avg <- data %>%
    dplyr::mutate(
      year = lubridate::year(!!date) %>% as.integer(),
      month = lubridate::month(!!date) %>% as.integer(),
      nb_d_month = lubridate::days_in_month(!!date) %>% as.integer()
      ) %>%
  dplyr::group_by(id, year, month) %>%
  dplyr::summarise(data = mean(!!var, na.rm = TRUE), nobs = sum(!is.na(!!var)),
    pc_obs = nobs / unique(nb_d_month)) #here
  # Filrer month that have enough record
  data_avg_cleaned <- data_avg %>%
    dplyr::mutate(
      data = replace(data, pc_obs < .5, NA),
      year_mon = lubridate::ymd(paste0(year, "-", month, "-", "01"))
      )
  # Identify pulse:
  pulse <- data_avg_cleaned %>%
    dplyr::group_by(!!id) %>%
    dplyr::summarise(p10 = quantile(data, low_threshold, na.rm = TRUE),
      p90 = quantile(data, high_threshold, na.rm = TRUE)
    )
  data_avg_cleaned %<>%
    dplyr::left_join(pulse, by = id_chr) %>%
    dplyr::mutate(
      low_pulse = ifelse(data < low_threshold, TRUE, FALSE),
      high_pulse = ifelse(data > high_threshold, TRUE, FALSE)
    ) %>%
    dplyr::group_by(id, year) %>%
    dplyr::summarise(
      low_pulse = ifelse(any(low_pulse), TRUE, FALSE),
      high_pulse = ifelse(any(high_pulse), TRUE, FALSE)
    )
  # 4. Complete the dataset with missing year:
  #For each id station, replace missing year by NA:
  test <- list(
    year = unique(data_avg_cleaned$year),
    id = unique(as.data.frame(donuts)[, id_chr])
  )
  comb_year_id <- expand.grid(test) %>%
    tibble::as_tibble()
  data_avg_complete <- comb_year_id %>%
    dplyr::left_join(data_avg_cleaned)

  data_avg_complete
}

#' Prepare ssn object
#'
#' Routine for openSTARS to create SSN object
#'  
#' @param mnt_path path to the mnt
#' @param pred_path path to the prediction sites
#' @param pred_name chr name of the prediction object
#' @param sites sf object or path to the obs sites
#' @param streams sf object or path to the hydrologic network 
#' @param ssn_path path to write the ssn folder
#' @param slope logical compute the slope attribute for pred and obs sites   
prepare_ssn <- function (grass_path = "/usr/lib/grass76/", mnt_path = NULL,
  pred_path = NULL, pred_name = NULL, sites = NULL, streams = NULL,
  ssn_path = NULL, slope = FALSE) {

  rgrass7::initGRASS(gisBase = grass_path,
    home = tempdir(),
    override = TRUE)
  openSTARS::setup_grass_environment(dem = mnt_path)
  openSTARS::import_data(dem = mnt_path,
    sites = sites,
    streams = streams,
    pred_sites = pred_path
  )
  openSTARS::derive_streams()
  #rgrass7::use_sp()

  cj <- openSTARS::check_compl_junctions()
  #cj <- openSTARS::check_compl_confluences()
  if(cj){
    #openSTARS::correct_compl_confluences()
    openSTARS::correct_compl_junctions()
  }

  openSTARS::calc_edges()

  # Compute slope from dem:
  if (slope) {
    openSTARS::execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
      parameters = list(
	elevation = "dem",
	slope = "slope"
	))
    openSTARS::calc_attributes_edges(input_raster = c("slope", "dem"),
      stat_rast = rep("mean", 2),
      attr_name_rast = c("avSlo", "avAlt")
    )
    if (!is.null(pred_path)) {
    openSTARS::calc_sites(pred_sites = paste0(pred_name, "_o"))
    openSTARS::calc_attributes_sites_approx(sites_map = "sites",
      input_attr_name = c("avSlo", "avAlt"),
      output_attr_name = c("avSloA", "avAltA"),
      stat = rep("mean", 2))
    openSTARS::calc_attributes_sites_approx(sites_map = pred_name,
      input_attr_name = c("avSlo", "avAlt"),
      output_attr_name = c("avSloA", "avAltA"),
      stat = rep("mean", 2))
    }
  } else {
    if (!is.null(pred_path)) {
      openSTARS::calc_sites(pred_sites = paste0(pred_name, "_o"))
    } else {
      openSTARS::calc_sites()
    }
  }

  if (!is.null(pred_path)) {
    openSTARS::export_ssn(ssn_path, predictions = pred_name, delete_directory = TRUE)
  } else {
    openSTARS::export_ssn(ssn_path, delete_directory = TRUE)
  }
  rgrass7::unlink_.gislock()
}

#' Prepare basin data 
#'
#' @param
prepare_basin_data <- function (basin = NULL, group_var = NULL, streams = NULL,
  dem = NULL, obs_sites = NULL, pred_sites = NULL, crop_method = "crop",
  crs = 2154, save_path = mypath("data-raw", "ssn_interpolation"), buffer = FALSE, buf_size = 20000) {

  group_var <- rlang::enquo(group_var)

  streams %<>% sf::st_transform(crs = crs)
  obs_sites %<>% sf::st_transform(crs = crs)
  pred_sites %<>% sf::st_transform(crs = crs)

  #basin
  if (buffer) {
    basin <- rmapshaper::ms_simplify(input = basin, keep = .01) %>% 
      sf::st_as_sf()
  basin <- sf::st_buffer(basin, dist = buf_size)
  }
  basin %<>%
    dplyr::group_by(!!group_var) %>%
    tidyr::nest()

  write_basin_data <- function(name, data) {
    # crop streams, obs sites, pred sites
    croped_streams <- sf::st_crop(streams, data)
    sp_data  <- as(data, "Spatial")
    croped_dem <- raster::crop(dem, sp_data)

    if (crop_method == "crop") {
      croped_obs <- sf::st_crop(obs_sites, data)
      croped_pred <- sf::st_crop(pred_sites, data)
    } else if (crop_method == "mask") {
      int_obs <- sf::st_intersects(obs_sites, data)
      obs_mask <- purrr::map_lgl(int_obs, function(x) ifelse(length(x) > 0, TRUE, FALSE))
      croped_obs <- obs_sites[obs_mask, ]
      int_pred <- sf::st_intersects(pred_sites, data)
      pred_mask <- purrr::map_lgl(int_pred, function(x) ifelse(length(x) > 0, TRUE, FALSE))
      croped_pred <- pred_sites[pred_mask, ]
    }

    # Save
    basin_dir <- paste0(save_path, "/", name)
    if (!dir.exists(basin_dir)) {
      dir.create(basin_dir)
    }
    raster::writeRaster(dem,
      filename = paste0(basin_dir, "/", "dem.tif"),
      format="GTiff", overwrite=TRUE)
    sf::write_sf(croped_pred, paste0(basin_dir, "/", paste0(name, "_pred_sites.shp")))
    assign(paste0(name, "_streams"), croped_streams, envir = .GlobalEnv)
    assign(paste0(name, "_obs"), croped_obs, envir = .GlobalEnv)
    to_save <- c(paste0(name, "_streams"), paste0(name, "_obs"))
    mapply(save, list = to_save, file = paste0(basin_dir, "/", to_save, ".rda"), MoreArgs =
      list(compress = "bzip2"))
    invisible()
  }
  basin %>%
    dplyr::mutate(test = purrr::map2(!!group_var, data, write_basin_data))
}

#' Interpolate by basin
#' 
#' Interpolate data by basin
#' 
#' @param ssn_dir path to SSN
#' @param site_dir path to data 
#' @param data
interpolate_basin <- function(ssn_dir = mypath("data-raw", "ssn_interpolation"),
  basin_name = "nord", quality_data = NULL, var = c("TN", "TP"), cutoff_day = NULL, complete = TRUE) {

  pred_name <- paste0(basin_name, "_pred_sites")

  ssn_obj_path <- paste0(ssn_dir, "/", basin_name, ".ssn")
  ssn <- SSN::importSSN(ssn_obj_path, predpts = pred_name, o.write = TRUE)
  message(paste0("Importation of ", basin_name, ".ssn is done"))
  
  print(pred_name %in% names(names(ssn)))

# Compute the weight of each streams lines when they merged:
  ssn <- SSN::additive.function(ssn, "H2OArea",
    "afv_area")

# create distance matrix between pred and obs:
  SSN::createDistMat(ssn,
    predpts = pred_name, o.write = TRUE, amongpreds = TRUE)

  # Get quality yearly avg by donuts station:
  obs_data <- paste0(ssn_dir, "/", basin_name, "/", basin_name, "_obs.rda")
  load(obs_data)
  donuts <- get(paste0(basin_name, "_obs"))

  # Begin with nitrogen and phosphorus
  quality_data %<>%
    dplyr::filter(var_code %in% var)

  # Prepare data
  donuts %<>%
    dplyr::mutate(id = as.character(id))
  quality_data %<>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::group_by(var_code) %>%
    tidyr::nest()

  # Complete already existing dataset: 
  prediction_file <- paste0(ssn_dir, "/", basin_name, "/",
    "quality_prediction.rda")
  if (complete & file.exists(prediction_file)) {
    myload(quality_prediction, dir = paste0(ssn_dir, "/", basin_name))

    var_interpolated <- var[var %in% quality_prediction$var_code] 
    quality_data %<>%
      filter(!var_code %in% var_interpolated)
    message(paste0(cat(var_interpolated), " have been already interpolated.
	Please use complete = FALSE to override existing interpolation."))
    if (nrow(quality_data) == 0) {
      message("All the requested variables have been already interpolated.")
      return(NULL)
    }
    # To bind to the new later: 
    old_quality_prediction <- quality_prediction
    rm(quality_prediction)# To avoid strange behavior if interpolation fails later  
  }

  # Enable parallel computation:
  #source(mypath("analysis", "misc", "parallel_setup.R"))
  message(paste0("Data are ready to be summarise over years for ",
      cat(var[var %in% quality_prediction$var_code]),
      " variables."))
  quality_data %<>%
   dplyr::mutate(interp_data = purrr::map(data,
	~prepare_data_interpolation(data = .x, date = meas_date, var = value,
	  donuts = donuts, id = id, cutoff_day = cutoff_day)))
  # Filter data
  quality_data %<>%
    dplyr::select(var_code, interp_data)
  message(paste0("Data summarised."))
  # Interpolate

  message(paste0("Interpolation begins:"))
  tictoc::tic()
  quality_prediction <- quality_data %>%
    dplyr::group_by(var_code) %>%
    dplyr::mutate(interp_data = purrr::map(interp_data,
	~interpolate_ssn(data = .x, ssn = ssn, var = avg_data,
	  group = year, pred_name = pred_name)))
  res_time <- tictoc::toc()
  res_rime <- res_time$toc - res_time$tic
  message(paste0("Interpolations took ", res_rime, " sec."))

  quality_prediction %<>%
    tidyr::unnest(interp_data) %>%
    dplyr::select(-model, -data)

  if (complete & exists("old_quality_prediction")) {
    quality_prediction <- rbind(old_quality_prediction, quality_prediction)
  }
  
  mysave(quality_prediction, dir = paste0(ssn_dir, "/", basin_name), overwrite = TRUE)
}

interpolate_naiades <- function(ssn = NULL, basin = NULL, family = "Gaussian", var = "value", formula = "value ~ 1") {
  # Model
  model <- SSN::glmssn(
    formula = as.formula(formula),
    ssn.object = ssn,
    family = family,
    CorModels = c("LinearSill.tailup", "Mariah.taildown",
      "Exponential.Euclid"),
    addfunccol = "afv_area"
  )
  #if the model failed:
  if (class(model) == "list") {
    return(NA)
  }
  # Prediction
  pred_name <- paste0(basin, "_pred_sites")
  pred <- predict(model, pred_name)
  prediction <-
    pred$ssn.object@predpoints@SSNPoints[[1]]@point.data[, c("id", var, paste0(var, ".predSE"))]
  # CV
  cross_v <- SSN::CrossValidationStatsSSN(model)
  # Result
  return(list(cross_v = cross_v, prediction = prediction))
  }
fill_data_ssn <- function(ssn = NULL, data = NULL, var = NULL,
   enquo_var = FALSE, ...) {

  if (!is.character(var)) {
    if (enquo_var) {
    var <- rlang::enquo(var)
    }
    var <- rlang::quo_name(var)
  }

  # get ssn dataset to fill it
  data_id_var <- data[, c("id", var)]
  ssn_data <- ssn@obspoints@SSNPoints[[1]]@point.data

  # Suppress var if present
  if (var %in% colnames(ssn_data)) {
    ssn_data <- ssn_data[, ! names(ssn_data) %in% var, drop = F]
  }
  # Fill ssn with new data:
  #stopifnot(nrow(ssn_data) == nrow(data_id_var))
  ssn_data %<>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::left_join(data_id_var, by = "id")
  ssn@obspoints@SSNPoints[[1]]@point.data <- ssn_data

  return(ssn)
}
