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

# Get ssn
myload(ssn, dir = mypath("data-raw", "naiades_ssn"))
combin$ssn <- sapply(combin$basin, function (basin){
  return(ssn[[as.character(basin)]])
})


# Get data
options(mc.cores = 15)
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

data_dir <- mypath("data-raw", "polluants")
options(mc.cores = 10)
sapply(basin, function (basin) {

  combin <- filter(combin, basin == basin) %>%
    select(-data) %>%
    filter(parameter %in% unique(parameter)[76:length(unique(parameter))]) #1:75

  # if already data:
  obj_name <- paste0(basin, "_interp_mv_avg")
  obj_path <- mypath("data-raw", "polluants", paste0(obj_name,".rda"))
  if (file.exists(obj_path)) {
    # filter already done interpolation
    load(obj_path)
    assign("pre_data", get(paste0(obj_name)))
     combin %<>% 
       filter(
	 !(year %in% pre_data$year &
	   parameter %in% pre_data$parameter &
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
