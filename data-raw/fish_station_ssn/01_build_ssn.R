################################################################################
#                         Build ssn for fish stations                          #
################################################################################

library(raster)
library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
library(rgeos)
library(openSTARS)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

#########################
#  Test with one basin  #
#########################

# Load fish and environmental station
myload(rht, dir = mypath("data-raw"))
rht %<>% st_transform(crs = 2154)
myload(station_analysis, op_analysis, dir = mypath("data"))
station <- station_analysis %>%
  st_transform(crs = 2154) %>%
  select(id) %>%
  rename(station = id)

# Load the dem: 
file_mnt <- mypath(
  "data-raw",
  "dem_250m_lambert_93.tif")
dem <- raster::raster(file_mnt)
## crop dem to rht:  

## Save all datasets:
mysave(rht, station, dem,
  dir = mypath("data-raw", "fish_station_ssn"), overwrite = TRUE)
raster::writeRaster(dem,
  filename = mypath("data-raw", "fish_station_ssn", "dem.tif"),
  format="GTiff", overwrite=TRUE)
write_sf(station, mypath("data-raw", "fish_station_ssn", "station.shp"))

#######################
#  Create SSN object  #
#######################

prepare_ssn(grass_path = "/usr/lib/grass76/",
  mnt_path = mypath("data-raw", "fish_station_ssn", "dem.tif"),
  pred_path = NULL, pred_name = NULL,
  sites = station, streams = rht,
  ssn_path = mypath("data-raw", "fish_station_ssn", "fish_station.ssn"), slope = FALSE)


#####################
#  Save ssn object  #
#####################
ssn <- SSN::importSSN(mypath("data-raw", "fish_station_ssn", "fish_station.ssn"), o.write = TRUE)
# Compute the weight of each streams lines when they merged:
ssn <- SSN::additive.function(ssn, "H2OArea",
  "afv_area")
# create distance matrix between pred and obs:
SSN::createDistMat(ssn, o.write = TRUE)
mysave(ssn, dir = mypath("data-raw", "fish_station_ssn"), overwrite = TRUE)
