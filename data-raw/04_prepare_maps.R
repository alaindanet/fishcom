################################################################################
#                     Prepare maps for efficient plotting                      #
################################################################################

library(tidyverse)
library(magrittr)
library(sf)
library(rmapshaper)
devtools::load_all()

#######################
#  France and region  #
#######################

region <- read_sf("france_region_shp/regions-20180101.shp") %>%
  dplyr::filter(surf_km2 > 10000, nom != "Guyane")

# Simplify polygons
#https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
simplepolys <- rmapshaper::ms_simplify(input = region, tol) %>% #Can be used also for lines
  st_as_sf()
plot(simplepolys[, "nom"])

region_polygon <- select(simplepolys, nom)
devtools::use_data(region_polygon, overwrite = TRUE)

#############
#  Station  #
#############

# Change projection:
#https://www.rdocumentation.org/packages/sf/versions/0.7-2/topics/st_transform
station <- read_delim("fishing_station_localisation_wsg84.csv",
  delim = ";", locale = locale("fr", decimal_mark = "."),
  col_types = cols(ST_CODECSP = col_character()))
# Lambert II
xy_station <- dplyr::select(station, ST_ABCISSE, ST_ORDONNEE, ST_LOCALISATION, ST_ID)
## Get Lambert II code
library(rgdal)
EPSG <- make_EPSG()
EPSG[grep("Lambert zone II", EPSG$note), 1:2]
## Define CRS
station <- sf::st_as_sf(station, coords = c("ST_ABCISSE", "ST_ORDONNEE"), crs = 27572)

# Get the same projection than region polygon 
data(region_polygon)
station <- st_transform(station, st_crs(region_polygon))
plot(st_geometry(region_polygon))
plot(station, add = TRUE, pch = 20, col = "red")
## Save station
st_write(station, "station_wgs84.shp")

############
#  Stream  #
############

stream <- read_sf("cours_eau_shp/CoursEau_FXX.shp")
# Long time to run:
simplestream <- rmapshaper::ms_simplify(input = sample_n(stream ,100), keep = .01) %>% 
  st_as_sf()
rm(stream)
object.size(simplestream)
plot(simplestream[, "gid"])

################
#  SSN object  #
################
# See openSTARS
# https://github.com/MiKatt/openSTARS
library(raster)
#get MNT
file_mnt <- "BDALTIV2/1_DONNEES_LIVRAISON_2018-01-00246/BDALTIV2_MNT_250M_ASC_LAMB93_IGN69_FRANCE/BDALTIV2_250M_FXX_0098_7150_MNT_LAMB93_IGN69.asc"
mnt <- raster::raster(file_mnt)
# Lambert 93: http://spatialreference.org/ref/epsg/rgf93-lambert-93/ 
crs(mnt) <-  CRS('+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
plot(mnt)

## To continue with
#- cours_eau_shp
#- obstacles
# Match points to lines:
#https://github.com/r-spatial/sf/issues/790
#https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/
