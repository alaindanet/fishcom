library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "press_methods.R"))

myload(environmental_data, dir = mypath("data"))
env <- environmental_data %>%
  filter(opcod %in% op_analysis$opcod) %>%
  select(opcod, alt, slope, distsource) %>%
  left_join(
    select(env_analysis, opcod, width_river, avg_depth_station),
    by = "opcod") %>%
  left_join(select(ungroup(op_analysis), opcod, station, year), by = "opcod") %>%
  filter(!is.na(station), !is.na(year))

#DBO
myload(press_metrics, dir = mypath("data"))
dbo <- press_metrics %>%
  filter(category %in% c("DBO")) %>%
  select(station = id, year, dbo = press)

#temperature:
myload(yearly_temp_press_interp_mv_avg, dir = mypath("data-raw", "temp"))
temp <- yearly_temp_press_interp_mv_avg %>%
  group_by(id, year) %>%
  arrange(value.predSE) %>%
  slice(1) %>%
  select(station = id, year, temp = value_corrected) %>%
  mutate(temp = ifelse(temp > 45, NA, temp)) %>%
  ungroup()
#flow:
myload(yearly_flow_press_interp_mv_avg, dir = mypath("data-raw", "flow"))
flow <- yearly_flow_press_interp_mv_avg %>%
  group_by(id, year) %>%
  arrange(value.predSE) %>%
  slice(1) %>%
  select(station = id, year, flow = value_corrected) %>%
  ungroup()

env_metrics <- env %>%
  left_join(dbo, by = c("station", "year")) %>%
  left_join(temp, by = c("station", "year")) %>%
  left_join(flow, by = c("station", "year"))
mysave(env_metrics, dir = "~/Documents/post-these/mnhn/fishcom_biomass_dynamics/data")
