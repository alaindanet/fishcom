################################################################################
#                               Merge pressure                                 #
################################################################################

library('tidyverse')
library('magrittr')
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))


# Habitat description
myload(hab_analysis, temporal_station_desc, dir = mypath("data"))

# put them to factor:
lvl <- c("null", "weak", "medium", "high")
lvl_sinuosite <- c("straight", "sinuous", "very_sinuous", "meandering")
lvl_shade <- c("clear", "quite_clear", "quite_covered", "covered")

habitat_station <- hab_analysis %>%
  select(-opcod, -aqua_vg, - shade, - sinuosite) %>%
  mutate_if(is.character, list(~factor(., levels = lvl, ordered = TRUE))) %>%
  left_join(select(hab_analysis, station, shade, sinuosite)) %>%
  mutate(
    shade = factor(shade, levels = lvl_shade, ordered = TRUE),
    sinuosite = factor(sinuosite, levels = lvl_sinuosite, ordered = TRUE)) %>%
  group_by(station) %>%
  summarise_all(list(med = ~median(as.integer(.), na.rm = TRUE) %>% as.integer())) %>%
  left_join(select(temporal_station_desc, station, width_river_mean, avg_depth_station_mean))
# Put back var in factor
names(lvl_shade) <- seq(1,4)
names(lvl_sinuosite) <- seq(1,4)
names(lvl) <- seq(1,4)
temporal_habitat_station <- habitat_station %>%
  mutate(
    shade_med = recode_factor(shade_med, !!!lvl_shade, .ordered = TRUE),
    sinuosite_med = recode_factor(sinuosite_med, !!!lvl_sinuosite, .ordered = TRUE)) %>%
  mutate_at(
    vars(hole_pit_med, under_bank_med, rock_shelter_med, logjam_stumps_med, aqua_vg_shelter_med, edge_vg_med),
    list(~recode_factor(., !!!lvl, .ordered = TRUE)))
mysave(temporal_habitat_station, dir = mypath("data"))

############################
#  Environmental pressure  #
############################
# Flow, polluant and temperature

# polluants
myload(press, dir = mypath("data"))
press %<>%
  spread(category, press)

myload(press_temperature, dir = mypath("data-raw", "naiades_temperatures"))
myload(press_flow, dir = mypath("data-raw", "flow"))

temporal_evt_press <- press %>%
  left_join(press_temperature) %>%
  left_join(press_flow)
mysave(temporal_evt_press, dir = mypath("data"), overwrite = TRUE)

