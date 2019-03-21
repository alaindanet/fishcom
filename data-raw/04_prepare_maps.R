################################################################################
#                     Prepare maps for efficient plotting                      #
################################################################################

library(tidyverse)
library(readxl)
library(magrittr)
library(sf)
library(lwgeom)
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
station <- st_transform(station, crs = 4326)
plot(st_geometry(region_polygon))
plot(station, add = TRUE, pch = 20, col = "red")
## Save station
st_write(station, "station_wgs84.shp", delete_layer = TRUE)

data(op_analysis)
good_station_id <- op_analysis$station %>% unique
station_analysis <- filter(station, ST_ID %in% good_station_id)
devtools::use_data(station_analysis, overwrite = TRUE)

############
#  Stream  #
############

stream <- read_sf("cours_eau_shp/CoursEau_FXX.shp")
stream <- st_transform(stream, crs = 4326)
object.size(stream) * 10^-6
# drop 3D layer (necessary to save in shp)
stream <- st_zm(stream)
write_sf(stream, "synchrony/stream_shp/stream_wgs84.shp")
# Long time to run:
simplestream <- rmapshaper::ms_simplify(input = sample_n(stream ,100), keep = .01) %>% 
  st_as_sf()
rm(stream)
object.size(simplestream)
plot(simplestream[, "gid"])

##########
#  DEM   #
##########
# Get and reproject MNT according to WGS84

#http://r-sig-geo.2731867.n2.nabble.com/issue-in-using-projectRaster-in-raster-library-td7586147.html
library(raster)
library(gdalUtils)

#get MNT
file_mnt <- "BDALTIV2/1_DONNEES_LIVRAISON_2018-01-00246/BDALTIV2_MNT_250M_ASC_LAMB93_IGN69_FRANCE/BDALTIV2_250M_FXX_0098_7150_MNT_LAMB93_IGN69.asc"
mnt <- raster::raster(file_mnt)
# Lambert 93: http://spatialreference.org/ref/epsg/rgf93-lambert-93/ 
crs(mnt) <-  CRS('+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
# Write temp correct DEM with CRS specification 
writeRaster(mnt, "temp_dem_250m_lambert_93.tif")

# Convert dem with gdal wrapper
## Spec files and crs:
in_mnt <- "temp_dem_250m_lambert_93.tif"
out_mnt <- "DEM_230m_wgs84.tif"
##http://www.spatialreference.org/ref/epsg/4326/proj4/
crs_4326 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
## Change projection:
gdalwarp(
  srcfile       = in_mnt,
  dstfile       = out_mnt,
  t_srs         = crs_4326,
  output_Raster = TRUE,
  overwrite     = TRUE,
  multi         = TRUE,
  #tr            = c(10,10),
  r             = "bilinear",
  verbose       = TRUE
)
file.remove("temp_dem_250m_lambert_93.tif")

################
#  SSN object  #
################
# See openSTARS
# https://github.com/MiKatt/openSTARS


mnt <- projectRaster(mnt, crs = crs_4326)
plot(mnt)

## To continue with
#- cours_eau_shp
#- obstacles
# Match points to lines:
#https://github.com/r-spatial/sf/issues/790
#https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/

##############
#  DCE data  #
##############

# Localisation des Masses d'eau
dce <- read_sf("./DCE/2016/SurfaceWaterBodyLine_FR_20170410/SurfaceWaterBodyLine_FR_20170410.gml")
colnames(dce)
arrange(dce, thematicIdIdentifier)

# Load data 
## Caracterisation of the streams
file_rapportage <- "./DCE/2016/rapportage_2016.xlsx"
na_spec <- c("Not applicable", "Unknown", "Unknown2010", "No information", "MonitoredButNotUsed")
excel_sheets(file_rapportage)
eco_status <- read_excel(file_rapportage, sheet = "ESU_Etat_Eco", na = na_spec) %>%
  filter(surfaceWaterBodyCategory == "RW") %>%
  select_at(vars(contains("surfaceWaterBody"), ends_with("StatusOrPotentialValue")))
colnames(eco_status) %<>%
  str_replace_all(., "QE.{5}(-| |\\d){1,5}", "") %>%
  str_replace_all(., ":[a-zA-Z]+", "") %>%
  str_replace_all(., "\\s(C|c)onditions|status", "") %>%
  str_replace_all(., "\\s(C|c)onditions|status", "") %>%
  tolower %>%
  str_replace_all(., ".*$\\s", "") %>%
  str_replace_all(., "\\s", "_") %>%
  str_replace_all(., "river_basin_", "")

eco_status %<>%
  rename(
    id = surfacewaterbodycode,
    acidification = acidification_
  )
nb_na <- eco_status %>%
  summarise_all(list(na = ~length(which(is.na(.))))) %>%
  unlist

eco_status %<>% select(which(nb_na < 10000))

## change id to the map
dce %<>% rename(id = thematicIdIdentifier) %>%
  filter(id %in% eco_status$id)
## match eco-status to the map 
dce %<>% select(id) %>%
  left_join(., eco_status, by = "id")
## fit station in eco_status
### Get stations matching
data(station_analysis)
dce <- st_transform(dce, crs = st_crs(station_analysis))
nearest_lines_id <- st_nearest_feature(station_analysis, dce)
nearest_lines <- dce[nearest_lines_id,]
dist_station_stream <- st_distance(station_analysis, nearest_lines)
### Get the shortest distance for each station
### Match station to the nearest river
