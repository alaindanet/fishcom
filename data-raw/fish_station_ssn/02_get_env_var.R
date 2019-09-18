################################################################################
#                         Get environmental variables                          #
################################################################################

library(raster)
library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
library(rgeos)
library(openSTARS)
library(SSN)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

myload(station, rht, dir = mypath("data-raw", "fish_station_ssn"))

rgrass7::use_sp()
prepare_ssn(grass_path = "/usr/lib/grass76/",
  mnt_path = mypath("data-raw", "fish_station_ssn", "dem.tif"),
  pred_path = NULL, pred_name = NULL,
  sites = station, streams = rht,
  ssn_path = mypath("data-raw", "fish_station_ssn", "fish_station.ssn"), slope = TRUE)

ssn <- SSN::importSSN(mypath("data-raw", "fish_station_ssn", "fish_station.ssn"), o.write = TRUE)
# Compute the weight of each streams lines when they merged:
ssn <- SSN::additive.function(ssn, "H2OArea",
  "afv_area")
# create distance matrix between pred and obs:
SSN::createDistMat(ssn, o.write = TRUE)
mysave(ssn, dir = mypath("data-raw", "fish_station_ssn"), overwrite = TRUE)


# Collect relevant variables
myload(ssn, dir = mypath("data-raw", "fish_station_ssn"))
ssn_df <- getSSNdata.frame(ssn) 
ssn_df %<>%
  as_tibble() %>%
  dplyr::select(station, upDist, avSloA, avAltA)

# Collect latitude and longitude 
myload(station_analysis, dir = mypath("data"))
station_analysis %<>%
  st_transform(crs = 2154) %>%
  rename(station = id)


ids <- st_drop_geometry(station_analysis) %>%
  dplyr::select(station) 

geo_station <- cbind(ids, st_coordinates(station_analysis)) %>%
  left_join(ssn_df) %>%
  rename(long = X, lat = Y, source_dist = upDist, slope = avSloA, alt = avAltA)

mysave(geo_station, dir = mypath("data"), overwrite = TRUE)
