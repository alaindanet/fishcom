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
myload(local_yearly_pulse_temp, dir = mypath("data-raw", "temp"))


year_span <- unique(local_yearly_pulse_temp$year)
combin <- expand.grid(list(basin = basin, year = year_span)) %>%
  as_tibble() %>%
  arrange(desc(basin))

# Get ssn
myload(ssn, dir = mypath("data-raw", "fire_ssn", "temperature"))
combin$ssn <- sapply(combin$basin, function (basin){
  return(ssn[[as.character(basin)]])
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
MoreArgs = list(.data = local_yearly_pulse_temp,
  ssn_dir = mypath("data-raw", "fire_ssn", "temperature"))
)

# Loop to compute the two types of pulse:
for (pulse_type in c("low_pulse", "high_pulse")) {

# Get data in SSN
options(mc.cores = 1)
combin$ssn <- mcMap(fill_data_ssn,
  ssn = combin$ssn, data = combin$data,
  MoreArgs = list(var = paste0("nb_",pulse_type),
   enquo_var = FALSE)
)

#####################
#  Interpolate SSN  #
#####################

data_dir <- mypath("data-raw", "temp")
options(mc.cores = 30)
sapply(basin, function (basin) {

  combin <- filter(combin, basin == basin) %>%
    select(-data)

  # if already data:
  obj_name <- paste0(basin, "_interp_nb_", pulse_type)
  obj_path <- mypath("data-raw", "temp", paste0(obj_name,".rda"))
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
      family = "Poisson",
      formula = paste0("nb_", pulse_type, " ~ 1"),
      var = paste0("nb_", pulse_type)
    ),
    mc.preschedule = FALSE
  )

  if (file.exists(obj_path)) {
    combin <- rbind(pre_data, combin)
  }

  assign(paste0(obj_name), combin)

  save(list = paste0(obj_name), file = obj_path)
  message(paste0("Interpolation done for basin: ", basin))
  gc()
  return(NULL)
})


#####################
#  Extract results  #
#####################

# Object names:
pred_obj <- paste0("yearly_temp_local_", pulse_type, "_interp")
cv_obj <- paste0("cv_temp_local_", pulse_type, "_interp")

# If already done, skip the loop below:
if (all(file.exists(paste0(mypath("data-raw", "temp"), "/", c(pred_obj, cv_obj), ".rda")))) {
  cat(paste0("Results for ", pulse_type, " has been already gathered.\n"))
  break 
}


data_list <- lapply(basin, function (x) {
  obj <- paste0(x, "_interp_nb_", pulse_type)
  load(mypath("data-raw", "temp", paste0(obj, ".rda")))
  assign("data", get(obj))
  return(data)
})
# Merge them:
interpolation <- do.call(rbind, data_list)

fill_empty_cross_v <- names(interpolation$ssn[[3]][["cross_v"]])
fill_empty_pred <- names(interpolation$ssn[[3]][["prediction"]])
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
    prediction = map2(ssn, basin, function(x, basin) {
  if(is.null(names(x))) {
    ids <- id_by_basin[which(id_by_basin$basin == basin), ]$id
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
prediction <- prediction[, c("id", "year", paste0("nb_", pulse_type), paste0("nb_", pulse_type, ".predSE"))]
assign(pred_obj, prediction)
cross_v <- interpolation %>%
  unnest(cross_v) %>%
  select(-ssn, -prediction)
assign(cv_obj, cross_v)

sapply(list(pred_obj, cv_obj), function (x) {
  save(list = x, file = mypath("data-raw", "temp", paste0(x, ".rda")))
  })


dist_yearly_temp_local_pulse <- local_yearly_pulse_temp %>%
  gather(pulse, nb, nb_low_pulse, nb_high_pulse) %>%
  group_by(pulse) %>%
  summarise(min = min(nb), max = max(nb))


# Filter abberant values:
options(mc.cores = 15)
library(parallel)
prediction$value_corrected <- mcMap(
  function (x, distri) {
    check <- distri %>%
      unlist
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
  x = prediction[[paste0("nb_",pulse_type)]],
  MoreArgs = list(
    distri = filter(dist_yearly_temp_local_pulse, pulse == paste0("nb_", pulse_type)) %>%
      select(min, max)
  )
)

prediction$value_corrected <- 
  sapply(prediction$value_corrected, function(x) x[1])

save(list = pred_obj, file = mypath("data-raw", "temp", paste0(pred_obj, ".rda")))

}
