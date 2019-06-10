################################################################################
#                      Interpolation environmental datas                       #
################################################################################

# Fishing station are not the same than environmental station
# So, we will try to interpolate the environment data with kriging.
# To do this, we will use the kringing tools of SSN package which takes in
# account the topology of the network. 
# It takes a hydrologic network, a DEM, observation sites and prediction sites.
# The publication of the method: https://www.jstatsoft.org/article/view/v056i03

library(raster)
library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
library(rgeos)
library(openSTARS)
mypath <- rprojroot::find_package_root_file
Sys.setenv(LANG = "en")
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

##############
#  Get data  #
##############

# Load fish and environmental station
myload(rht, dir = mypath("data-raw"))
rht %<>% st_transform(crs = 2154)
myload(station_analysis, dir = mypath("data"))
station <- station_analysis %>%
  st_transform(crs = 2154) %>%
  select(id, drain_id)
myload(donuts_analysis, dir = mypath("data"))
donuts <- donuts_analysis %<>%
  st_transform(crs = 2154) %>%
  select(id, drain_id) %>%
  mutate(id = as.character(id))

# Load the dem: 
file_mnt <- mypath(
  "data-raw",
  "dem_250m_lambert_93.tif")
dem <- raster::raster(file_mnt)
## crop dem to rht:  
rht_sp <- as(rht, "Spatial")
dem <- raster::crop(dem, raster::extent(rht_sp))
plot(dem)


## Save all datasets:
mysave(rht, donuts, dir = mypath("data-raw", "ssn_interpolation"),
  overwrite = TRUE)
writeRaster(dem,
  filename = mypath("data-raw", "ssn_interpolation", "dem.tif"),
  format = "GTiff", overwrite = TRUE)
write_sf(station, mypath("data-raw", "ssn_interpolation", "station.shp"))

#######################
#  Create SSN object  #
#######################

# Set dem path
file_mnt <- mypath("data-raw", "ssn_interpolation", "dem.tif")
file_pred <- mypath("data-raw", "ssn_interpolation", "station.shp")

#1. Init GRASS:
initGRASS(gisBase = "/usr/lib/grass72/",
          home = tempdir(),
          override = TRUE)
setup_grass_environment(dem = file_mnt)
# 2. Import the data
import_data(dem = file_mnt,
  sites = donuts,
  streams = rht,
  pred_sites = file_pred
)
# 3. Compute streams
derive_streams()
#derive_streams(burn = 100, accum_threshold = 700,
  #condition = TRUE, clean = TRUE)

cj <- check_compl_junctions()
if (cj) {
  correct_compl_junctions()
}
# 4. Compute edges 
calc_edges()
# 5. Compute slope from dem:
execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          parameters = list(
            elevation = "dem",
            slope = "slope"
          ))
# 5.1. for edges: 
calc_attributes_edges(input_raster = c("slope", "dem"),
  stat_rast = rep("mean", 2),
  attr_name_rast = c("avSlo", "avAlt")
                      )
calc_sites(pred_sites = "station_o")
# 5.2. for obs sites and pred (station) sites
calc_attributes_sites_approx(sites_map = "sites",
                             input_attr_name = c("avSlo", "avAlt"),
                             output_attr_name = c("avSloA", "avAltA"),
                             stat = rep("mean", 2))
calc_attributes_sites_approx(sites_map = "station",
                             input_attr_name = c("avSlo", "avAlt"),
                             output_attr_name = c("avSloA", "avAltA"),
                             stat = rep("mean", 2))

# 6. Save data 
ssn_dir <- mypath("data-raw", "ssn_interpolation", "donuts_station.ssn")
export_ssn(ssn_dir, predictions = "station", delete_directory = TRUE)

# 7. Get data and visualise:
# 7.1. Get and inspect data
dem <- readRAST("dem", ignore.stderr = TRUE)
slope <- readRAST("slope", ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data, n = 4)
sites <- readVECT("sites", ignore.stderr = TRUE)
head(sites@data, n = 4)
site_o <- readVECT("sites_o", ignore.stderr = TRUE)
pred_sites_o <- readVECT("station_o", ignore.stderr = TRUE)
pred_sites <- readVECT("station", ignore.stderr = TRUE)
head(pred_sites@data, n = 4)
# 6.2. Plot (can be time consuming) 
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites, pch = 20, col = "black")
points(pred_sites, pch = 20, col = "white")
png()

unlink_.gislock()

###################################
#  Prepare SSN for interpolation  #
###################################

library(SSN)

ssn_dir <- mypath("data-raw", "ssn_interpolation", "donuts_station.ssn")
ssn <- importSSN(ssn_dir, predpts = "station", o.write = TRUE)
names(ssn)

# Compute the weight of each streams lines when they merged: 
names(ssn@data)
ssn <- additive.function(ssn, "H2OArea",
  "afv_area")

# create distance matrix between pred and obs:
createDistMat(ssn,
  predpts = "station", o.write = TRUE, amongpreds = TRUE)

# Get flow avg by donuts station:
myload(flow_data, dir = mypath("data-raw"))
flow_avg_complete <- prepare_data_interpolation(data = flow_data,
  date = meas_date, var = value, donuts = donuts, id = id)

# The best correlation structure is: LinearSill.tailup + Mariah.taildown
undebug(interpolate_ssn)
debug(compute_glmssn)
test <- interpolate_ssn(ssn = ssn,
  data = filter(flow_avg_complete, year == 1998), group = year, var = avg_data)
