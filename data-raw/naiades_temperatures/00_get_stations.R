################################################################################
#                             Get Naiade stations                              #
################################################################################

library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

station <- read_delim(mypath("data-raw", "naiades_temperatures", "stations.csv"),
  delim = ";", locale = locale("fr", decimal_mark = "."))
replacement_col <- c(
  "CdStationMesureEauxSurface" = "id",
  "CoordXStationMesureEauxSurface" = "x",
  "CoordYStationMesureEauxSurface" = "y",
  "CdProjStationMesureEauxSurface" = "code_proj",
  "LibelleProjection" = "lib_proj"
)
xy_station <- station[, names(station) %in% names(replacement_col)]
colnames(xy_station) <- str_replace_all(colnames(xy_station), replacement_col) 

library(rgdal)
EPSG <- make_EPSG()
EPSG[grep("RGF93 / Lambert-93", EPSG$note), 1:2]
## Define CRS
station_naiades_temperature <- sf::st_as_sf(xy_station, coords = c("x", "y"), crs = 2154) %>%
  select(id)

mysave(station_naiades_temperature, dir = mypath("data-raw", "naiades_temperatures"), overwrite = TRUE)
