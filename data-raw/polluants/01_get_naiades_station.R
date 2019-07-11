################################################################################
#                       Get Naiades station localisation                       #
################################################################################


library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data <- read_delim(
  file = mypath("data-raw", "polluants", "naiades_data", "stations.csv"),
  delim = ";", locale = locale(decimal_mark = "."))

replacement_col <- c(
 CdStationMesureEauxSurface = "id",
 LbStationMesureEauxSurface = "name",
 CoordXStationMesureEauxSurface = "lon",
 CoordYStationMesureEauxSurface = "lat",
 LibelleProjection = "projection",
 LbCommune = "commune",
 LbDepartement = "dept",
 LbRegion = "region",
 NomCoursdEau = "stream",
 AltitudePointCaracteritisque = "alt"
 )

data <- data[, names(replacement_col)]
colnames(data) <- str_replace_all(colnames(data), replacement_col)

data %<>%
  filter(projection == "RGF93 / Lambert 93")

station_naiades <- st_as_sf(data, coords = c("lon", "lat"), crs = 2154)

#Remove corsica
myload(region_polygon, dir = mypath("data"))
region_polygon %<>% st_transform(crs = 2154)
int_naiades <- sf::st_intersects(station_naiades, region_polygon)
obs_mask <- purrr::map_lgl(int_naiades, function(x) ifelse(length(x) > 0, TRUE, FALSE))
station_naiades <- station_naiades[obs_mask, ]

mysave(station_naiades, dir = mypath("data-raw"), overwrite = TRUE)
#proj_code <- c(
#`RGF93 / Lambert 93` = 2154, #Metropole
#`RGR92 / UTM 40` = 2975, #Reunion 
#`RRAF 91 (WGS84) / UTM 20` = 4559, #French Antilles
#`RGFG95 / UTM 22` = 2972 #French Guiana 
#)

