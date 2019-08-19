################################################################################
#                      Test interpolation over Sein basin                      #
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
source(mypath("R", "plot_methods.R"))

#########################
#  Test with one basin  #
#########################

# Load fish and environmental station
myload(rht, dir = mypath("data-raw"))
rht %<>% st_transform(crs = 2154)
myload(station_analysis, dir = mypath("data"))
station_analysis %<>% st_transform(crs = 2154)
myload(donuts_analysis, dir = mypath("data"))
donuts_analysis %<>% st_transform(crs = 2154)

# Get attribute of the rht: 
attr_rht <- read_delim(
  mypath("data-raw", "RHT", "Attributs_RHT_fev2011_Phi_qclass2.txt"),
  delim = "\t"
)
unique(attr_rht$region_csp)
## Add to rht the region:
rht %<>%
  rename(id_drain = ID_DRAIN) %>%
  left_join(dplyr::select(attr_rht, id_drain, region_csp), by = "id_drain")

# Select donuts, fish station and rht that are in Seine basin:
rht_sein <- rht %>%
  filter(region_csp == "Sein")
donuts_sein <- donuts_analysis %>%
  mutate(id = as.character(id)) %>%
  filter(drain_id %in% rht_sein$id_drain)
station_sein <- station_analysis %>%
  filter(drain_id %in% rht_sein$id_drain)

# Get flow avg by donuts station:
myload(flow_data, dir = mypath("data-raw"))
flow_data %<>%
  filter(id %in% donuts_sein$id)
flow_avg_complete <- prepare_data_interpolation(data = flow_data,
  date = meas_date, var = value, donuts = donuts_sein, id = id)
mysave(flow_avg_complete, dir = mypath("data-raw", "interpolation_test"), overwrite = TRUE)
# Put it to donuts:
donuts_sein %<>%
  left_join(filter(flow_avg_complete, year == 1998), by = "id")

# Load the dem: 
file_mnt <- mypath(
  "data-raw",
  "dem_250m_wgs84.tif")
dem <- raster::raster(file_mnt)
## crop dem to rht:  
rht_sein_sp <- as(rht_sein, "Spatial")
dem_sein <- raster::crop(dem, rht_sein_sp)

## Save all datasets:
mysave(rht_sein, donuts_sein, station_sein, dem_sein,
  dir = mypath("data-raw", "interpolation_test"), overwrite = TRUE)
writeRaster(dem_sein,
  filename = mypath("data-raw", "interpolation_test", "dem_sein.tif"),
  format="GTiff", overwrite=TRUE)
write_sf(station_sein, mypath("data-raw", "interpolation_test", "station_sein.shp"))

#######################
#  Create SSN object  #
#######################

# Set dem path
file_mnt <- mypath("data-raw", "interpolation_test", "dem_sein.tif")
file_pred <- mypath("data-raw", "interpolation_test", "station_sein.shp")
initGRASS(gisBase = "/usr/lib/grass76/",
          home = tempdir(),
          override = TRUE)
setup_grass_environment(dem = file_mnt)
import_data(dem = file_mnt,
  sites = donuts_sein,
  streams = rht_sein,
  pred_sites = file_pred
)
derive_streams()

dem <- readRAST("dem", ignore.stderr = TRUE)
calc_edges()
edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data, n = 4)

# Compute slope from dem:
execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          parameters = list(
            elevation = "dem",
            slope = "slope"
          ))
calc_attributes_edges(input_raster = c("slope", "dem"),
  stat_rast = rep("mean", 2),
  attr_name_rast = c("avSlo", "avAlt")
                      )
calc_sites(pred_sites = "station_sein_o")
calc_attributes_sites_approx(sites_map = "sites",
                             input_attr_name = c("avSlo", "avAlt"),
                             output_attr_name = c("avSloA", "avAltA"),
                             stat = rep("mean", 2))
calc_attributes_sites_approx(sites_map = "station_sein",
                             input_attr_name = c("avSlo", "avAlt"),
                             output_attr_name = c("avSloA", "avAltA"),
                             stat = rep("mean", 2))
sites <- readVECT("sites", ignore.stderr = TRUE)
head(sites@data, n = 4)
site_o <- readVECT("sites_o", ignore.stderr = TRUE)
pred_sites_o <- readVECT("station_sein_o", ignore.stderr = TRUE)
pred_sites <- readVECT("station_sein", ignore.stderr = TRUE)
head(pred_sites@data, n = 4)
names(pred_sites)

#plot
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites, pch = 20, col = "black")
points(pred_sites, pch = 20, col = "white")


ssn_dir <- mypath("data-raw", "interpolation_test", "seine.ssn")
export_ssn(ssn_dir, predictions = "station_sein", delete_directory = TRUE)

unlink_.gislock()

##########################
#  Exploratory analysis  #
##########################
library(SSN)

ssn_dir <- mypath("data-raw", "interpolation_test", "seine.ssn")
seine_ssn <- importSSN(ssn_dir, predpts = "station_sein", o.write = TRUE)
names(seine_ssn)

# Compute the weight of each streams lines when they merged: 
names(seine_ssn@data)

# Save ssn again
export_ssn(ssn_dir, predictions = "station_sein", delete_directory = TRUE)

# Check if distance matrix has been saved
seine_ssn <- importSSN(ssn_dir, predpts = "station_sein", o.write = TRUE)

# Semi-variogram for flow:
flow_vario <- Torgegram(seine_ssn, "avg_flow", nlag = 20)
plot(flow_vario)

mod0 <- glmssn(flow ~ upDist, seine_ssn,
 CorModels = NULL, use.nugget = TRUE)
summary(mod0)
CrossValidationStatsSSN(mod0)

mod_sp0 <- glmssn(flow ~ upDist, seine_ssn,
 CorModels = c("Exponential.Euclid"), use.nugget = TRUE)
CrossValidationStatsSSN(mod_sp0)

mod_sp <- glmssn(avg_flow ~ 1, seine_ssn,
 CorModels = c("LinearSill.tailup", "Mariah.taildown",
 "Exponential.Euclid"), addfunccol = "afv_area")
summary(mod_sp)
CrossValidationStatsSSN(mod_sp)

# Prediction:
pred_station <- predict(mod_sp, "station_sein")

# Check
plot(pred_station, SEcex.max = 1, SEcex.min = 0.5/3 * 2
 )

# Check:  
cv.out <- CrossValidationSSN(mod_sp)
par(mfrow = c(1, 2))
plot(mod_sp$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(seine_ssn)[, "avg_flow"]), cv.out[, "cv.se"],
pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")
# Compare AIC:
mod_sp1 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("Exponential.tailup", "Exponential.taildown"),
 addfunccol = "afv_area")
mod_sp2 <- glmssn(avg_avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("LinearSill.tailup", "Mariah.taildown"),
 addfunccol = "afv_area")
mod_sp3 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("LinearSill.tailup", "LinearSill.taildown"),
 addfunccol = "afv_area")
mod_sp4 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("Mariah.tailup", "LinearSill.taildown"),
 addfunccol = "afv_area")
mod_sp5 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("Mariah.tailup", "Mariah.taildown"),
 addfunccol = "afv_area")
mod_sp6 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = c("Spherical.tailup", "Spherical.taildown"),
 addfunccol = "afv_area")
mod_sp7 <- glmssn(avg_flow ~ upDist + avSloA, seine_ssn,
 CorModels = "Exponential.Euclid",
 addfunccol = "afv_area")

InfoCritCompare(list(mod_sp1, mod_sp2, mod_sp3,
 mod_sp4, mod_sp5, mod_sp6, mod_sp7))

# The best correlation structure is: LinearSill.tailup + Mariah.taildown
myload(flow_avg_complete, dir = mypath("data-raw", "interpolation_test"))
filter(flow_avg_complete, !is.na(avg_data))

undebug(interpolate_ssn)
names(seine_ssn)
test <- interpolate_ssn(ssn = seine_ssn, data = flow_avg_complete, group = year, var = avg_data, pred_name = "station_sein")

#######################################
#  Test interpolation and prediction  #
#######################################


debug(prepare_pulse_interpolation)
flow_pulse_complete <- prepare_pulse_interpolation(data = flow_data,
  date = meas_date, var = value, donuts = donuts_sein, id = id)
filter(flow_pulse_complete, !is.na(low_pulse))
flow_pulse_complete %<>%
  mutate(low_pulse = as.numeric(low_pulse),
    high_pulse = as.numeric(high_pulse)
  )
summary(flow_pulse_complete)

binSp$ssn.object@data %>%
  as_tibble()
mf04p
test <- getSSNdata.frame(mf04p, Name = "Obs")
names(test)
str(test[, "MaxOver20"])
binSp <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
       CorModels = c("Mariah.tailup", "Spherical.taildown"),
       family = "binomial", addfunccol = "afvArea")
undebug(compute_glmssn)
predict.glmssn

test <- interpolate_ssn(formula = low_pulse ~ avSloA,
  ssn = seine_ssn, data = filter(flow_pulse_complete, year == 1998),
  family = "binomial", group = year, var = low_pulse)
unnest(test, prediction)
