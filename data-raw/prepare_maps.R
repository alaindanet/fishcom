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
st_geometry
plot(st_geometry(region_polygon))
plot(station, add = TRUE, pch = 20, col = "red")
## Save station
st_write(station, "station_wgs84.shp")

## To continue with
#- cours_eau_shp
#- obstacles
# Match points to lines:
#https://github.com/r-spatial/sf/issues/790
#https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/
#
