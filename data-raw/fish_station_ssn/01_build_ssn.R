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
myload(station_analysis, dir = mypath("data"))
station <- station_analysis %>%
  st_transform(crs = 2154)

# Load the dem: 
file_mnt <- mypath(
  "data-raw",
  "dem_250m_wgs84.tif")
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

# Set dem path
file_mnt <- mypath("data-raw", "fish_station_ssn", "dem.tif")
initGRASS(gisBase = "/usr/lib/grass76/",
          home = tempdir(),
          override = TRUE)
setup_grass_environment(dem = file_mnt)
import_data(dem = file_mnt,
  sites = station,
  streams = rht
)
derive_streams()

cj <- openSTARS::check_compl_junctions()
#cj <- openSTARS::check_compl_confluences()
if(cj){
  #openSTARS::correct_compl_confluences()
  openSTARS::correct_compl_junctions()
}

calc_edges()
# Compute slope from dem:
#execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          #parameters = list(
            #elevation = "dem",
            #slope = "slope"
          #))
#calc_attributes_edges(input_raster = c("slope", "dem"),
  #stat_rast = rep("mean", 2),
  #attr_name_rast = c("avSlo", "avAlt")
                      #)
#calc_attributes_sites_approx(sites_map = "sites",
                             #input_attr_name = c("avSlo", "avAlt"),
                             #output_attr_name = c("avSloA", "avAltA"),
                             #stat = rep("mean", 2))

calc_sites()
ssn_dir <- mypath("data-raw", "fish_station_ssn", "fish_station.ssn")
export_ssn(ssn_dir, delete_directory = TRUE)

unlink_.gislock()
prepare_ssn()

#####################
#  Save ssn object  #
#####################
ssn <- SSN::importSSN(ssn_dir, o.write = TRUE)
# Compute the weight of each streams lines when they merged:
ssn <- SSN::additive.function(ssn, "H2OArea",
  "afv_area")
# create distance matrix between pred and obs:
SSN::createDistMat(ssn, o.write = TRUE)
mysave(ssn, dir = mypath("data-raw", "fish_station_ssn"), overwrite = TRUE)
