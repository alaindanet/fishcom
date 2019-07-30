#################################
#  Flow interpolation by basin  #
#################################

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
myload(yearly_avg_flow, dir = mypath("data-raw", "flow"))

year_span <- unique(yearly_avg_flow$year)
parameter <- unique(yearly_avg_flow$parameter)
combin <- expand.grid(list(basin = basin, year = year_span, parameter = parameter)) %>%
  as_tibble() %>%
  arrange(desc(basin))

# Get ssn
myload(ssn, dir = mypath("data-raw", "fire_ssn", "flow"))
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
MoreArgs = list(.data = yearly_avg_flow,
  ssn_dir = mypath("data-raw", "fire_ssn", "flow"))
)

# Get data in SSN
options(mc.cores = 1)
combin$ssn <- mcMap(fill_data_ssn,
  ssn = combin$ssn , data = combin$data,
  MoreArgs = list(var = "value",
   enquo_var = FALSE)
)

#####################
#  Interpolate SSN  #
#####################

data_dir <- mypath("data-raw", "flow")
options(mc.cores = 15)
sapply(basin, function (basin) {

  combin <- filter(combin, basin == basin) %>%
    select(-data) %>%

  # if already data:
  obj_name <- paste0(basin, "_interp_mv_avg")
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
