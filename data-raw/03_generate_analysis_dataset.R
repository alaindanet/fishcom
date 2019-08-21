################################################################################
#                    Generate the dataset for the analysis                     #
################################################################################

library(tidyverse)
library(magrittr)
library(lubridate)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "fishing_op_build")
source(mypath("R", "building_dataset.R"))
source(mypath("R", "misc.R"))


myload(op, dir = mypath("data-raw"))

#op_sp_ind Summary of op 
myload(op_sp_ind, dir = mypath("data"))
op_sp_ind
op %<>% left_join(op_sp_ind, by = "opcod")

op %<>%
  mutate(
    protocol_type = ifelse(protocol == "complete", "complete", "partial"),
    date = lubridate::date(date) #date-time to date
    ) %>%
  select(opcod, date, station, protocol, protocol_type, surface, nb_sp, nb_ind)

op_clean <- op %>%
  group_by(protocol_type) %>%
  nest() %>%
  mutate(clean = purrr::map(data,
      ~rm_dbl_fishing_op(op = .x, sep_threshold = 270, nb_sampling = 10))) %>%
  select(-data)

# Check that there are not double station:
station_protocol <- sapply(op_clean$clean, function (x) {
 unique(x$station) 
}) %>% unlist()
length(unique(station_protocol)) == length(station_protocol)

opcod_clean <- op_clean %>%
  unnest() %>%
  .[["opcod"]]
  
# Test for the length fished
myload(op_desc, dir = mypath("data-raw"))
op_desc %<>%
  rename(opcod = ope_id) %>%
  filter(opcod %in% opcod_clean)
op_clean %<>%
  unnest() %>%
  left_join(select(op_desc, opcod, length_sourced)) %>%
  arrange(station) 

op_cv_length <- op_clean %>%
  group_by(station) %>%
  summarise(
    mean = mean(length_sourced),
    median = median(length_sourced),
    cv = mean / sd(length_sourced),
    cv = replace(cv, cv == Inf, 0)
  )
test_too_different <- op_clean %>%
  left_join(op_cv_length, by = "station") %>%
  mutate(out = ifelse(length_sourced > median + .3 * median, TRUE, FALSE))
filter(test_too_different, out == TRUE) %>%
  filter(protocol != "complete") %>%
  arrange(station)
filter(op_clean, station == 345)

op_analysis <- op_clean %>%
  filter(opcod %in% filter(test_too_different, out == FALSE)$opcod)

op_hist <- op_analysis %>%
  group_by(station) %>%
  summarise(n = n()) %>%
  filter(n >= 10)
op_analysis %<>% 
  filter(station %in% op_hist$station)

mysave(op_analysis, dir = mypath("data"), overwrite = TRUE)

########################
#  Environmental data  #
########################

myload(op_env, dir = mypath("data-raw"))
myload(op_analysis, dir = mypath("data"))
myload(op_hab, dir = mypath("data-raw"))
good_opcod_id <- select(ungroup(op_analysis), opcod) %>% unlist

op_env %<>%
  filter(opcod %in% good_opcod_id) %>%
  left_join(select(op_analysis, opcod, station)) %>%
  left_join(op_desc)
summary(op_env)

env_analysis <- op_env
mysave(env_analysis, dir = mypath("data"), overwrite = TRUE)
rm(env_analysis, op_env, op_desc)

op_hab %<>%
  filter(opcod %in% good_opcod_id) %>%
  left_join(select(op_analysis, opcod, station))

hab_analysis <- op_hab
mysave(hab_analysis, dir = mypath("data"), overwrite = TRUE)
rm(hab_analysis, op_hab)


#######################
#  Clean fish length  #
#######################

myload(fish_length, dir = mypath("data"))
myload(op_analysis, dir = mypath("data"))

# Select good op
fish_length %<>% filter(opcod %in% good_opcod_id)
summary(fish_length)

# Remove accidental species
nb_ind_sp <- fish_length %>%
  group_by(species) %>%
  summarise(nind = n())
low_nb_ind <- filter(nb_ind_sp, nind <= 100)
fish_length %<>% filter(!species %in% low_nb_ind$species)

# Remove crazy length
filter(fish_length, length > 5000)
distri_sp_length <- fish_length %>%
  group_by(species) %>%
  summarise(
    avg = mean(length, na.rm = TRUE),
    sdt = sd(length, na.rm = TRUE))
# Remove it
fish_length %<>%
  left_join(distri_sp_length) %>%
  mutate(length = ifelse(length > avg + 5 * sdt, NA, length)) %>%
  select(-avg, -sdt)

length_analysis <- fish_length

mysave(length_analysis, dir = mypath("data"), overwrite = TRUE)
rm(fish_length, length_analysis)

##################
#  Plot station  #
##################

# get their localisation
myload(station, dir = mypath("data-raw"))
station_analysis <- station %>% filter(id %in% op_analysis$station)
# get the map
station_analysis %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
myload(region_polygon, dir = mypath("data"))
plot(st_geometry(region_polygon), lwd = 1.5, col = "grey85")
plot(st_geometry(station_analysis), pch = 20, col = "red", add = T)

mysave(station_analysis, dir = mypath("data"), overwrite = TRUE)
write_sf(station_analysis,mypath("data-raw", "station", "station_analysis_wgs84.shp"))


