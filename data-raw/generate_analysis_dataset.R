################################################################################
#                    Generate the dataset for the analysis                     #
################################################################################

library(tidyverse)
library(magrittr)
library(cowplot)
library(ggpmisc)
library(sf)
library(rgdal)
library(lubridate)
devtools::load_all()
theme_set(theme_alain())


data(operation_data)
op <- dplyr::select(operation_data, opcod:year, count)
rm(operation_data)

# Summary of op 
op_stat <- op %>%
  group_by(opcod, station, year, month) %>%
  summarise(
    nb_ind = sum(count),
    nb_sp  = n()
    )

# Add details about the op 
data(operation_data)
op_method <- operation_data %>%
  distinct(opcod, .keep_all = TRUE) %>%
  select(opcod, nbpass:strategy)
op <- left_join(op_stat, op_method, by = "opcod")

#plot rep of methods
qplot(times, data = op_method, fill = method) +
  labs(x = "Time of monthly repeated sampling")

op %<>% filter(
  !(method == "complete" & nbpass == 1), #Rm complete that has only 1 passage
  method != "other"#Rm other method
)

# Numbering the sampling events by station
op_hist <- op %>%
  group_by(station) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(station = fct_inorder(as.factor(station)))
# good station IDs
good_station <- op_hist %>%
  dplyr::filter(freq >= 10)
good_station_id <- good_station %>%
  dplyr::select(station) %>%
  unlist(., use.names = FALSE)
length(good_station_id)


# For temporal analysis, we keep station followed more than 10 times
op <- filter(op, station %in% good_station_id)

op_analysis <- op
devtools::use_data(op_analysis, overwrite = TRUE)

good_opcod_id <- select(ungroup(op_analysis), opcod) %>% unlist 

data(environmental_data)
env_analysis <- filter(environmental_data, opcod %in% good_opcod_id)
devtools::use_data(env_analysis, overwrite = TRUE)
rm(environmental_data, env_analysis)

data(fish_length)
length_analysis <- filter(fish_length, opcod %in% good_opcod_id)
devtools::use_data(length_analysis, overwrite = TRUE)
rm(fish_length, length_analysis)



######################
#  Further analysis  #
######################

# Get the time between each sampling event  
int_op <- op %>%
  ungroup() %>%
  unite(year_month, year, month, sep = "-") %>%
  mutate(times = ymd(paste0(year_month, "-01"))) %>%
  select(-year_month) %>%
  group_by(station) %>%
  arrange(times) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, times[-1] - times[-length(station)])
  )

low_int <- filter(int_op, sample_sep < 60) %>%
  ungroup() %>%
  arrange(station)
filter(int_op, station == 591) %>%
  slice(10:20)
# If sample_sep = 0, if sp < 4 | nb_ind < 100 -> merge the operation code
#  

qplot(times, data = low_int, fill = method) +
  labs(x = "Time of monthly repeated sampling")
# Look careful to point_big_mil: big sampled surface and less fish than the
# other ?


##################
#  Plot station  #
##################

# get their localisation
station <- read_delim("fishing_station_localisation_wsg84.csv",
  delim = ";", locale = locale("fr", decimal_mark = "."),
  col_types = cols(ST_CODECSP = col_character()))
xy_station <- dplyr::select(station, XCOORD, YCOORD)
station <- SpatialPointsDataFrame(coords = xy_station, data = station,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# get the map
station <- st_as_sf(station)
loc_good_station <- dplyr::filter(station, ST_ID %in% good_station_id)
region_fr <- raster::shapefile("france_region_shp/regions-20180101.shp")
region_fr <- st_as_sf(region_fr) #easier to manipulate
region_to_filter <- c("La Réunion", "Martinique", "Guadeloupe", "Mayotte",
  "Guyane", "Corse")
region_fr <- filter(region_fr, !(nom %in% region_to_filter))
region_fr <- region_fr[, c("nom", "geometry")]
plot(st_geometry(region_fr), lwd = 1.5, col = "grey85")
plot(loc_good_station, pch = 20, col = "red", add = T)
