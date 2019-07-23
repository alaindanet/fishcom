################################################################################
#                               Merge pressure                                 #
################################################################################

library('tidyverse')
library('magrittr')


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
# Environmental pressure
myload(press, dir = mypath("data-raw", "ssn_interpolation"))
press %<>%
  spread(var_code, press) %>%
  select(-DOC, -TOC)
cor(na.omit(press[, !names(press) == "id"]))
press %<>% rename(station = id)

myload(temporal_station_desc, dir = mypath("data"))
temporal_station_desc %<>%
  select(
    -width_river_mean,
    -avg_depth_station_mean,
    -width_river_cv,
    -avg_depth_station_cv)

test <- left_join(press, temporal_station_desc) %>%
  left_join(temporal_habitat_station) %>%
  ungroup
test %<>%
  mutate_if(is.factor, ~as.integer(.))

library('ade4')
temporal_habitat_station
pca <- dudi.pca(as.data.frame(na.omit(select(test, -station))), scannf = FALSE, nf = 4, center = TRUE, scale = TRUE)

screeplot(pca)
summary(pca)
scatter(pca, posieig = "none")
s.corcircle(pca$co)
s.corcircle(pca$co, xax = 1, yax = 3)
s.corcircle(pca$co, xax = 2, yax = 3)

