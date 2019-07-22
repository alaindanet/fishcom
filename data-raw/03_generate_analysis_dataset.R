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
#devtools::load_all()
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "fishing_op_build")
source(mypath("R", "plot_methods.R"))
source(mypath("R", "misc.R"))
theme_set(theme_alain())


myload(op, dir = mypath("data-raw"))

#op_sp_ind Summary of op 
myload(op_sp_ind, dir = mypath("data"))
op_sp_ind

op %<>% left_join(op_sp_ind, by = "opcod")

##############################################
#  Distribution of fishing operation timing  #
##############################################


qplot(month, data = mutate(op, month = month(date))) +
  labs(x = "Date",
    y = "Frequency")


######################################
#  Remove doubled fishing operation  #
######################################

# Get the time between each sampling event
int_op <- op %>%
  ungroup() %>%
  group_by(station) %>%
  arrange(date) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, date[-1] - date[-length(station)])
  ) %>%
  arrange(station)

## double_station 
low_int <- filter(int_op, sample_sep < 60) %>%
  ungroup() %>%
  arrange(station)
filter(int_op, station == 657)

## filter double sampling, surely doubled 
clean_dbl <- group_by(int_op, station) %>%
  nest() %>%
  mutate(data = map(data, keep_most_complete_sampling)) %>%
  unnest()


# Check new time separation between 2 consecutive ops  
clean_dbl %<>%
  group_by(station) %>%
  arrange(date) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, date[-1] - date[-length(station)])
  ) %>%
  arrange(station)
qplot(sample_sep, data = filter(clean_dbl, sample_sep < 2000)) +
  labs(x = "Number of days between two fishing operation",
    y = "Frequency") +
  xlim(c(0, 2000))
filter(clean_dbl, sample_sep < 160)
filter(clean_dbl, station == 709)
filter(clean_dbl, station == 657)

# Numbering the sampling events by station
op_hist <- clean_dbl %>%
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
qplot(x = freq, data = good_station, geom = "histogram")

# Test fish month distribution 
op_test <- filter(op, station %in% good_station_id) %>%
  mutate(month = month(date))
qplot(month, data = op_test) +
  labs(x = "Date",
    y = "Frequency")
## By station
op_test_station  <- op_test %>%
  group_by(station) %>%
  summarise(avg = mean(month), sdt = sd(month))
qplot(sdt, data = op_test_station) +
  labs(x = "Date",
    y = "Frequency")


# For temporal analysis, we keep station followed more than 10 times
op <- filter(op, station %in% good_station_id)
op %<>%
  mutate(year = year(date))

op_analysis_complete_partial <- op
devtools::use_data(op_analysis_complete_partial, overwrite = TRUE)

op %<>% filter( protocol == "complete")
op_analysis <- op
devtools::use_data(op_analysis, overwrite = TRUE)

good_opcod_id <- select(ungroup(op_analysis), opcod) %>% unlist

################################
#  Test for the length fished  #
################################

# Test for the length fished
myload(op_desc, dir = mypath("data-raw"))
myload(op_analysis, dir = mypath("data"))
op_desc %<>%
  rename(opcod = ope_id) %>%
  filter(opcod %in% good_opcod_id)
op_analysis %<>%
  left_join(select(op_desc, opcod, length_sourced)) %>%
  arrange(station)

op_cv_length <- op_analysis %>%
  group_by(station) %>%
  summarise(
    mean = mean(length_sourced),
    median = median(length_sourced),
    cv = mean / sd(length_sourced),
    cv = replace(cv, cv == Inf, 0)
  )
test_too_different <- op_analysis %>%
  left_join(op_cv_length, by = "station") %>%
  mutate(out = ifelse(length_sourced > median + .3 * median, TRUE, FALSE))
filter(test_too_different, out == TRUE) %>%
  arrange(station)

filter(op_analysis, station == 203)

op_analysis %<>% filter(opcod %in% filter(test_too_different, out == FALSE)$opcod)

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
devtools::use_data(env_analysis, overwrite = TRUE)
rm(env_analysis, op_env, op_desc)

op_hab %<>%
  filter(opcod %in% good_opcod_id) %>%
  left_join(select(op_analysis, opcod, station))

hab_analysis <- op_hab
devtools::use_data(hab_analysis, overwrite = TRUE)
rm(hab_analysis, op_hab)


#######################
#  Clean fish length  #
#######################

myload(fish_length, dir = mypath("data"))

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
filter(fish_length, length > 1000)
## TRF distri 
filter(fish_length, species == "TRF") %>%
  summary()
# Remove it
fish_length %<>%
  filter(!length > 10000) # Also drop NA 

length_analysis <- fish_length

devtools::use_data(length_analysis, overwrite = TRUE)
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

###################
#  Pressure data  #
###################

myload(press_prob_avg, press_cat_med, dir = mypath("data-raw"))
myload(op_analysis, dir = mypath("data"))

press_prob_avg %<>%
  filter(station %in% op_analysis$station)
press_cat_med %<>%
  filter(station %in% op_analysis$station)
temporal_press_prob_analysis <- press_prob_avg
temporal_press_cat_analysis <- press_cat_med
mysave(temporal_press_prob_analysis, temporal_press_cat_analysis,
  dir = mypath("data"), overwrite = TRUE)

