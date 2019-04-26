####################################
#  Build temporal habitat dataset  #
####################################
library('tidyverse')
library('magrittr')

myload(env_analysis, dir = data_common)
colnames(env_analysis)

col_hydro <- c("opcod", "station", "slope", "width_river", "width",
  "avg_depth_station", "avg_daily_flow", "isolation_up", "isolation_down",
  "chanelled_station")
hydro <- env_analysis %>%
  select(col_hydro)

###############
#  Isolation  #
###############
## The different modality of isolation
hydro %>%
  select(isolation_down, isolation_up) %>%
  summarise_all(list(test = ~list(unique(.)))) %>%
  unnest()

## Number of times by station where isolation has been reported:
isolation <- hydro %>%
  select(station, isolation_down, isolation_up) %>%
  mutate_at(vars(matches("isolation")), list(~ifelse(. == "no", "no", "yes"))) %>%
  group_by(station) %>%
  summarise_all(list(~paste(ifelse(any(. == "yes"), "yes", "no"))))

##########
#  Flow  #
##########

# Ok, we have no quantitative flow data
sum(is.na(hydro$avg_daily_flow)) / nrow(hydro)
sum(hydro$avg_daily_flow == 0, na.rm = TRUE) / nrow(hydro)

flow_col <- c("instream_flow", "artificial_flow_variation", "replenishment_flow")
flow <- env_analysis %>%
  select(station, flow_col) %>%
  group_by(station) %>%
  summarise_all(list(~paste(ifelse(any(. == "yes"), "yes", "no"))))

#############################
#  Morphology of the river  #
#############################

sum(is.na(hydro$slope)) / nrow(hydro)
sum(hydro$slope == 0, na.rm = TRUE) / nrow(hydro)
# We have no slope

river_morpho <- hydro %>%
  select(station, slope, width_river, avg_depth_station) %>%
  group_by(station) %>%
  summarise_all(list(mean = ~mean(., na.rm = TRUE), cv = ~sd(., na.rm = TRUE)/ mean(., na.rm = TRUE)))

hydro %>%
  select(chanelled_station) %>%
  summarise_all(list(test = ~list(unique(.)))) %>%
  unnest()

# NA if unknown 
channel <- hydro %>%
  select(station, chanelled_station) %>%
  rename(channelled = chanelled_station) %>%
  mutate(channelled = replace(channelled, channelled == "unknown", NA)) %>%
  group_by(station) %>%
  summarise_all(list(~paste(ifelse(any(. == "yes"), "yes", "no"))))

###########
#  Usage  #
###########
# Resettlement, sports, etc...
col_usage <- c("resettlement", "resettlement_observations",
"sailed_station", "nautical_sports", "dredging")

usage <- env_analysis %>%
  select(station, col_usage) %>%
  select(-resettlement_observations) %>%
  group_by(station) %>%
  summarise_all(list(~paste(ifelse(any(. == "yes"), "yes", "no"))))

# Good to see resettlement_observations, very informative
# Would be nice to find a way to extract those informations
env_analysis %>%
  select(station, resettlement_observations)

###############
#  Merge all  #
###############

temporal_station_desc <- left_join(isolation, flow, by = "station") %>%
  left_join(channel, by = "station") %>%
  left_join(usage, by = "station") %>%
  left_join(river_morpho, by = "station")

mysave(temporal_station_desc, dir = data_common, overwrite = TRUE)

#############
#  Habitat  #
#############
library("ade4")
library("adegraphics")

myload(hab_analysis, dir = data_common)

habitat <- hab_analysis %>%
  mutate_all(list(as.factor)) %>%
  select(-opcod, -station, -aqua_vg)

mca <- dudi.acm(as.data.frame(habitat), scannf = FALSE, nf = 5)

screeplot(mca)
summary(mca)
scatter(mca)

# Try to average station values with median  
unique(hab_analysis$hole_pit)
unique(hab_analysis$shade)
unique(hab_analysis$sinuosite)
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
  summarise_all(list(med = ~median(as.numeric(.), na.rm = TRUE)))


## Join habitat with shade and sinuosite 
pca <- dudi.pca(as.data.frame(select(habitat_station, -station)), scannf = FALSE, nf = 2, center = TRUE, scale = TRUE)

screeplot(pca)
summary(pca)
scatter(pca, posieig = "none")
s.corcircle(pca$co)
biplot(pca)
pca$li
pca$tab
pca

## Habitat and stability
myload(temporal_community_metrics, dir = data_common) 
stab <- temporal_community_metrics %>%
  select(station, biomass_stab)

hab_stab <- left_join(habitat_station, stab)
qplot(x = biomass_stab, data = hab_stab, geom = "histogram")

all(hab_stab$station == habitat_station$station)
hab_stab %<>%
  mutate(axis1 = pca$li[[1]], axis2 = pca$li[[2]])

### variable 
var_pca <- tibble(
  variable = rownames(pca$co),
  comp1 = 5 * pca$co[[1]],
  comp2 = 5 * pca$co[[2]])

p <- ggplot(data = hab_stab, aes(x = axis1, y = axis2, size = biomass_stab)) +
  geom_point() +
  geom_text(data = var_pca, aes(x = comp1, y = comp2, label = variable), inherit.aes = FALSE, size = 6)
myscree(pca1$eig / sum(pca1$eig))


model <- lm(biomass_stab ~ hole_pit_med + under_bank_med + rock_shelter_med +
  logjam_stumps_med + aqua_vg_shelter_med + edge_vg_med + shade_med +
  sinuosite_med, data = hab_stab)

model_pca <- lm(hab_stab$biomass_stab ~ )
plot(model_pca)
