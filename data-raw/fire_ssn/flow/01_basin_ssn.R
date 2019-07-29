################################################################################
#                       Prepare ssn file for each basin                        #
################################################################################


library(raster)
library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

# Load fish and environmental station
myload(rht, dir = mypath("data-raw"))
rht %<>% st_transform(crs = 2154)
# Fish station:
myload(station, dir = mypath("data-raw"))
station %<>% as_tibble()
myload(op_analysis_complete_partial, dir = mypath("data"))
station_analysis <- station %>%
  filter(id %in% unique(op_analysis_complete_partial$station))
# get the map
station_analysis %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
station_analysis %<>%
  st_transform(crs = 2154) %>%
  dplyr::select(id)
# Environmental station:
myload(donuts_analysis, dir = mypath("data"))
donuts_analysis %<>% st_transform(crs = 2154)

##########################################
#  Select environmental station to keep  #
##########################################

myload(yearly_avg_polluants, dir = mypath("data-raw", "polluants"))
unique(yearly_avg_polluants$id)
nobs_station <- yearly_avg_polluants %>%
  group_by(id, parameter) %>%
  summarise(nobs = n()) %>%
  group_by(id) %>%
  summarise(nobs = mean(nobs))

arrange(nobs_station, desc(nobs)) %>%
  filter(nobs >= 10)

# basin
basin <- read_sf(mypath("data-raw", "basin_dce", "BassinDCE.shp"))
basin %<>% st_transform(crs = 2154)
# Filter antilles
basin %<>% filter(CdBassinDC %in% c("B1", "B2", "A", "D", "F", "C", "G", "H"))
basin <- rmapshaper::ms_simplify(input = basin) %>%
  st_as_sf()
plot(st_geometry(basin))
nord <- basin %>%
  filter(CdBassinDC %in% c("B1", "B2", "C", "A", "H")) %>%
  st_union() %>%
  st_sf()
plot(st_geometry(nord))
basin2 <- basin %>%
  filter(!CdBassinDC %in% c("B1", "B2", "C", "A", "H"))
basin4 <- c(st_geometry(basin2), st_geometry(nord)) %>%
  st_sf() %>%
  mutate(basin_name = c("est", "sud", "ouest" , "nord"))
plot(basin4)

my_hydro_basin <- basin4
mysave(my_hydro_basin, dir = mypath("data-raw", "fire_ssn"),
  overwrite = TRUE)

# DEM
file_mnt <- mypath( "data-raw", "dem_250m_lambert_93.tif")
dem <- raster::raster(file_mnt)


myload(my_hydro_basin, dir = mypath("data-raw", "fire_ssn"))
# Create folder with appropriate data by basin
prepare_basin_data(basin = my_hydro_basin, group_var = basin_name, streams = rht,
  dem = dem, obs_sites = donuts_analysis, pred_sites = station_analysis,
  crs = 2154, save_path = mypath("data-raw", "fire_ssn"))

#options(mc.cores = 2)
sapply(my_hydro_basin$basin_name, function (basin) {
  # Prepare path:
  mnt_path <- mypath("data-raw", "fire_ssn", basin, "dem.tif") 
  pred_path <- paste0(mypath("data-raw", "fire_ssn", basin),
    "/", basin, "_pred_sites.shp")
  # load data:
  load(paste0(mypath("data-raw", "fire_ssn", basin), "/", basin,
      "_obs.rda"))
  load(paste0(mypath("data-raw", "fire_ssn", basin), "/", basin,
    "_streams.rda"))
  # Assign to object in the function:
  assign("streams", get(paste0(basin, "_streams")))
  assign("sites", get(paste0(basin, "_obs")))

  # Create appropriate ssn object
  prepare_ssn(grass_path = "/usr/lib/grass76/",
    mnt_path = mnt_path,
    pred_path = pred_path,
    pred_name = paste0(basin, "_pred_sites"),
    sites = sites, streams = streams,
    ssn_path = mypath("data-raw", "fire_ssn", paste0(basin, ".ssn")),
    slope = FALSE)
  })
