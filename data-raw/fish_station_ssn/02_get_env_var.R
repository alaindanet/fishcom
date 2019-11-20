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
  dplyr::select(station, avSloA, avAltA)

# Collect latitude and longitude 
myload(station_analysis, dir = mypath("data"))
station_analysis %<>%
  st_transform(crs = 2154) %>%
  rename(station = id)


ids <- st_drop_geometry(station_analysis) %>%
  dplyr::select(station) 

geo_station <- cbind(ids, st_coordinates(station_analysis)) %>%
  left_join(ssn_df) %>%
  rename(long = X, lat = Y, slope = avSloA, alt = avAltA)

# Add rht attributes
myload(attr_rht, dir = mypath("data-raw"))
attr_rht %<>%
  dplyr::select(id_drain, d_source, strahler, region_csp) %>%
  rename(ID_DRAIN = id_drain)
rht %<>%
  left_join(attr_rht, by = "ID_DRAIN")

match_station_rht <- match_pt_line(
  station_analysis, rht, start_buffer = 25, inc_buffer = 100)

station_analysis$ID_DRAIN <-
  rht$ID_DRAIN[unlist(match_station_rht)]
station_analysis %<>%
  left_join(st_drop_geometry(rht), by = "ID_DRAIN") %>%
  dplyr::select(station, d_source, strahler, region_csp)

geo_station %<>%
  left_join(station_analysis, by = "station") %>%
  dplyr::select(-geometry)

mysave(geo_station, dir = mypath("data"), overwrite = TRUE)
