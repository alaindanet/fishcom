################################################################################
#                         Interpolate nb of high pulse                         #
################################################################################

library(tidyverse)
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

#  Interpolation
library('SSN')
library(parallel)
basin <- c("ouest", "sud", "est", "nord") #"nord"
myload(local_yearly_pulse_flow, dir = mypath("data-raw", "flow"))


year_span <- unique(local_yearly_pulse_flow$year)
combin <- expand.grid(list(basin = basin, year = year_span)) %>%
  as_tibble() %>%
  arrange(desc(basin))

# Get ssn
myload(ssn, dir = mypath("data-raw", "fire_ssn", "flow"))
combin$ssn <- sapply(combin$basin, function (basin_chr){
  return(ssn[[as.character(basin_chr)]])
})

# Get data
options(mc.cores = 30)
combin$data <- mcMap(function (basin, year, .data, ssn_dir){

  ## Prepare interpolation data
  # Filter year:
  .data <- .data[.data$year == year, ]
  # Filter stations:
  station_file <- paste0(ssn_dir, "/", basin, "/", paste0(basin, "_obs"), ".rda")
  load(station_file) #name: `basin`_obs
  assign("station",get(paste0(basin, "_obs")))
  .data %<>%
    filter(id %in% station$id)
  rm(station)
  return(.data)

}, basin = combin$basin, year = combin$year,
MoreArgs = list(.data = local_yearly_pulse_flow,
  ssn_dir = mypath("data-raw", "fire_ssn", "flow"))
)

# Loop to compute the two types of pulse:
for (pulse_type in c("nb_low_pulse", "nb_high_pulse", "low_pulse", "high_pulse")) {
  var_chr <- pulse_type#paste0("nb_", pulse_type)

  if (pulse_type %in% c("nb_low_pulse", "nb_high_pulse")) {
    dist_family <- "Poisson" 
  } else if (pulse_type %in% c("low_pulse", "high_pulse")) {
    dist_family <- "Binomial" 
  } else {
    stop("Unknown variable.")
  }

# Get data in SSN
options(mc.cores = 1)
combin$ssn <- mcMap(fill_data_ssn,
  ssn = combin$ssn, data = combin$data,
  MoreArgs = list(var = var_chr,
   enquo_var = FALSE)
)

#####################
#  Interpolate SSN  #
#####################

data_dir <- mypath("data-raw", "flow")
options(mc.cores = 30)
sapply(basin, function (basin_chr) {

  combin <- filter(combin, basin == basin_chr) %>%
    select(-data)
  stopifnot(nrow(combin[!combin$basin %in% basin_chr, ]) == 0)

  # if already data:
  obj_name <- paste0(basin_chr, "_interp_", var_chr)
  obj_path <- mypath("data-raw", "flow", paste0(obj_name,".rda"))
  if (file.exists(obj_path)) {
    # filter already done interpolation
    load(obj_path)
    assign("pre_data", get(paste0(obj_name)))
     combin %<>%
       filter(
	 !(year %in% pre_data$year &
	   basin %in% pre_data$basin)
       )
  }
  # If all have been already interpolated:
  if (nrow(combin) == 0) {
    message("all have been already interpolated.")

    return(NULL)
  }
  combin$ssn <- mcMap(
    interpolate_naiades,
    ssn = combin[["ssn"]],
    basin = combin[["basin"]],
    MoreArgs = list(
      family = dist_family,
      formula = paste0(var_chr, " ~ 1"),
      var = var_chr 
    ),
    mc.preschedule = FALSE
  )

  if (file.exists(obj_path)) {
    combin <- rbind(pre_data, combin)
  }

  assign(paste0(obj_name), combin)

  save(list = paste0(obj_name), file = obj_path)
  message(paste0("Interpolation done for basin: ", basin_chr))
  gc()
  return(NULL)
})


#####################
#  Extract results  #
#####################

# Object names:
pred_obj <- paste0("yearly_flow_local_", var_chr, "_interp")
cv_obj <- paste0("cv_flow_local_", var_chr, "_interp")

# If already done, skip the loop below:
if (all(file.exists(paste0(mypath("data-raw", "flow"), "/", c(pred_obj, cv_obj), ".rda")))) {
  cat(paste0("Results for ", var_chr, " has been already gathered.\n"))
  next 
}


data_list <- lapply(basin, function (x) {
  obj <- paste0(x, "_interp_", var_chr)
  load(mypath("data-raw", "flow", paste0(obj, ".rda")))
  assign("data", get(obj))
  return(data)
})
# Merge them:
interpolation <- do.call(rbind, data_list)

list_names_id <- which(sapply(interpolation$ssn, class) == "list")[1]
fill_empty_cross_v <- names(interpolation$ssn[[list_names_id]][["cross_v"]])
fill_empty_pred <- names(interpolation$ssn[[list_names_id]][["prediction"]])
id_by_basin <- interpolation %>%
  group_by(basin) %>%
  slice(4) %>%
  mutate(id = map(ssn, function(x) x[["prediction"]][, "id"])) %>%
  select(basin, id)
# Get pred and data.frame

interpolation %<>%
  mutate(
    cross_v = map(ssn, function(x) {
  if(is.null(names(x))) {
    out <- data.frame(
      matrix(NA, nrow = 1, ncol = length(fill_empty_cross_v))
    )
    colnames(out) <- fill_empty_cross_v
    out
  } else {
    x[["cross_v"]]
  }
}),
    prediction = map2(ssn, basin, function(x, basin_chr) {
  if(is.null(names(x))) {
    ids <- id_by_basin[which(id_by_basin$basin == basin_chr), ]$id
    out <- data.frame(
      matrix(NA, nrow = length(ids), ncol = 3)
    )
    out[, 1] <- ids
    colnames(out) <- fill_empty_pred
    out
  } else {
    x[["prediction"]]
  }
})
  )
  
prediction <- interpolation %>%
  unnest(prediction)
prediction <- prediction[, c("id", "year", var_chr, paste0(var_chr, ".predSE"))]
assign(pred_obj, prediction)
cross_v <- interpolation %>%
  unnest(cross_v) %>%
  select(-ssn, -prediction)
assign(cv_obj, cross_v)

sapply(list(pred_obj, cv_obj), function (x) {
  save(list = x, file = mypath("data-raw", "flow", paste0(x, ".rda")))
  })


temp <- local_yearly_pulse_flow[, names(local_yearly_pulse_flow) %in% var_chr] %>%
  unlist
dist_yearly_flow_local_pulse <- c(min = min(temp), max = max(temp))

# Filter abberant values:
options(mc.cores = 15)
library(parallel)
prediction$value_corrected <- mcMap(
  function (x, distri) {
    check <- distri
    if (is.na(x)) {
      return(NA)
    } else if (x < check["min"]) {
      return(check["min"])
    } else if (x > check["max"]) {
      return(check["max"])
    } else {
      return(x)
    }
  },
  x = prediction[[var_chr]],
  MoreArgs = list(
    distri = dist_yearly_flow_local_pulse 
  )
)

prediction$value_corrected <- 
  sapply(prediction$value_corrected, function(x) x[1])

save(list = pred_obj, file = mypath("data-raw", "flow", paste0(pred_obj, ".rda")))

}
