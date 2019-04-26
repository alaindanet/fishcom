################################################################################
#                    Prepare data for flow and quality data                    #
################################################################################

library('tidyverse')
library('magrittr')
library('sf')
library('rgeos')
devtools::load_all()
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "flow_quality")

myload(stations_FR_river_donuts)
donuts <- read_sf(paste0(mydir, "/stations_FR_river_donuts.shp"))
donuts <- st_transform(donuts, crs = 4326)
st_crs(donuts)

myload(station, dir = mypath("data-raw"))
myload(op_analysis_complete_partial, dir = mypath("data"))
station <- st_as_sf(station, coords = c("lon", "lat"),  crs = 4326) %>%
  filter(id %in% op_analysis_complete_partial$station)

myload(region_polygon, dir = mypath("data"))

plot(st_geometry(region_polygon))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
plot(st_geometry(station), add = TRUE, pch = 20, col = "red")

# Buffer in dec degree: 0.1° = 11km
station_buffer <- st_buffer(station, dist = .1)
plot(st_geometry(station_buffer))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
donuts_in_station <- st_within(donuts, station_buffer)

station_with_donuts <- donuts_in_station %>%
  unlist(., use.names = FALSE) %>% unique
donuts_to_keep <- donuts_in_station %>%
  map_lgl(., function(x){ ifelse(length(x) > 0, TRUE, FALSE)})



plot(st_geometry(region_polygon))
plot(st_geometry(donuts[donuts_to_keep,]), add = TRUE, pch =
  20, col = "blue")
plot(st_geometry(station), add = TRUE, pch = 20, col = "green")
station_without_donuts <- slice(station, - station_with_donuts)
plot(st_geometry(station_without_donuts), add = TRUE, pch = 20, col = "red")

## There is 15 station without donuts: bigger buffers for them
station_buffer2 <- st_buffer(station_without_donuts, dist = .2)
plot(st_geometry(station_buffer2))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
donuts_in_station2 <- st_within(donuts, station_buffer2)

station_with_donuts2 <- donuts_in_station2 %>%
  unlist(., use.names = FALSE) %>% unique
length(station_with_donuts2)
donuts_to_keep2 <- donuts_in_station2 %>%
  map_lgl(., function(x) { ifelse(length(x) > 0, TRUE, FALSE)})
which(donuts_to_keep2 == TRUE)

donuts_final <- donuts_to_keep | donuts_to_keep2

which(donuts_final == TRUE) %>% length

donuts_analysis  <- slice(donuts, which(donuts_final == TRUE))
mysave(donuts_analysis, dir = mypath("data"))
