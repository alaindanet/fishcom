####################################
#  Build temporal habitat dataset  #
####################################

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
