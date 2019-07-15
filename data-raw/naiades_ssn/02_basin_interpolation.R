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
myload(yearly_avg_polluants, dir = mypath("data-raw", "polluants"))

year_span <- unique(yearly_avg_polluants$year)
parameter <- unique(yearly_avg_polluants$parameter)
combin <- expand.grid(list(basin = basin, year = year_span, parameter = parameter)) %>%
  as_tibble() %>%
  arrange(desc(basin))

options(mc.cores = 4)
ssn <- mclapply(basin, function(basin) {
  pred_name <- paste0(basin, "_pred_sites")
  ssn_dir <- mypath("data-raw", "naiades_ssn")
  ssn_obj_path <- paste0(ssn_dir, "/", basin, ".ssn")
  ssn <- SSN::importSSN(ssn_obj_path, predpts = pred_name, o.write = TRUE)
# Compute the weight of each streams lines when they merged:
  ssn <- SSN::additive.function(ssn, "H2OArea",
    "afv_area")
# create distance matrix between pred and obs:
  SSN::createDistMat(ssn,
    predpts = pred_name, o.write = TRUE, amongpreds = TRUE)
  ssn
})
names(ssn) <- basin 

# Get ssn
combin$ssn <- sapply(combin$basin, function (basin){
  return(ssn[[as.character(basin)]])
})

# Get data
options(mc.cores = 30)
combin$data <- mcMap(function (basin, year, parameter, data_dir, ssn_dir){
  obs_data <- paste0(data_dir, "/", parameter, ".rda")
  load(obs_data) #name of the object is data
  ## Prepare interpolation data
  # Filter year:
  data <- data[data$year == year, ]
  # Filter stations:
  station_file <- paste0(ssn_dir, "/", basin, "/", paste0(basin, "_obs"), ".rda")
  load(station_file) #name: `basin`_obs
  assign("station",get(paste0(basin, "_obs")))
  data %<>%
    filter(id %in% station$id)
  rm(station)
  return(data)

}, basin = combin$basin, year = combin$year, parameter = combin$parameter,
MoreArgs = list(data_dir = mypath("data-raw", "polluants",
    "yearly_parameter_data"),
  ssn_dir = mypath("data-raw", "naiades_ssn"))
)

#####################
#  Interpolate SSN  #
#####################


options(mc.cores = 10)
combin$result <- mcMap(interpolate_naiades,
  ssn = combin[["ssn"]],
  data = combin[["data"]],
  basin = combin[["basin"]],
  MoreArgs = list(
 var = "value"
  )
)

combin %<>%
  dplyr::select(-ssn, -data)

press_interpolation <- combin
mysave(press_interpolation, dir = mypath("data-raw", "polluants"), overwrite = TRUE)
