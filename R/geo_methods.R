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

  #Get data:
  data %<>% dplyr::group_by(!!group) %>%
    tidyr::nest()

  # Model, cross-validation and prediction
  data %<>%
    mutate(
      model = purrr::map(data, compute_glmssn, var = var, ssn = ssn, formula =
	formula, family = family, corModel = corModel),
      cross_v = purrr::map(model, function(x) {
	out <- SSN::CrossValidationStatsSSN(x)
	unlist(out)
	}),
      prediction = purrr::map(model, function(m){
	pred <- predict(m, pred_name)
	pred$ssn.object@predpoints@SSNPoints[[1]]@point.data[, c("id",var_chr, paste0(var_chr, ".predSE"))]
	})
    )
  data
}

#' Compute glmss model
#'
#' To feed in interpolate_ssn 
compute_glmssn <- function(data = NULL, var = NULL,
  ssn = NULL, formula = NULL, family = NULL, corModel = NULL, ...) {

  var_chr <- rlang::quo_name(var)

  # get ssn dataset to fill it
  data_id_var <- data %>%
    dplyr::select(id, !!var)
  ssn_data <- ssn@obspoints@SSNPoints[[1]]@point.data

  # Suppress var if present
  if (var_chr %in% colnames(ssn_data)) {
    ssn_data %<>% dplyr::select(-!!var)
  }
  # Fill ssn with new data:
  stopifnot(nrow(ssn_data) == nrow(data_id_var))
  ssn_data %<>%
    dplyr::left_join(data_id_var, by = "id")
  ssn@obspoints@SSNPoints[[1]]@point.data <- ssn_data

  # temporary hack:
  if (is.null(formula)) {
    formula <-  as.formula(paste0(var_chr," ~ 1"))
  }
  if (is.null(corModel)) {
    corModel <- c("LinearSill.tailup", "Mariah.taildown",
      "Exponential.Euclid")
  }
  if (is.null(family)) {
    family <- "Gaussian"
  }

  mod_sp <- SSN::glmssn(formula, ssn, family = family,
    CorModels = corModel, addfunccol = "afv_area")
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
