################################################################################
#                             Make press and pulse                             #
################################################################################

library(tidyverse)
library(magrittr)
library('stringr')
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "press_methods.R"))

###########
#  press  #
###########

myload(yearly_press_interp_mv_avg, dir = mypath("data-raw", "polluants"))
myload(op_analysis, dir = mypath("data"))

# Filter the year corresponding to each station:
year_station <- op_analysis %>% 
  group_by(station) %>%
  summarise(year_list = list(unique(year)))

# If several values have been interpolated, keep the safest
yearly_press_interp_mv_avg %<>%
  group_by(id, year, parameter) %>%
  arrange(value.predSE) %>%
  slice(1)

myload(press_cat, dir = mypath("data-raw", "polluants"))
myload(polluant_units, dir = mypath("data-raw", "polluants", "naiades_data"))

yearly_press_interp_mv_avg %<>%
  ungroup() %>%
  mutate(parameter = as.character(parameter)) %>%
  left_join(press_cat, by = "parameter")
missing_cat <- filter(yearly_press_interp_mv_avg, is.na(category))
stopifnot(nrow(missing_cat) == 0)
# Move aluminium which is more a polluant than acidification
yearly_press_interp_mv_avg %<>%
  mutate(
    category = ifelse(parameter == "aluminium", "micropolluants mineraux", category),
    category = ifelse(parameter == "ph", "ph", category) 
  )
# Keep only phosphore total:
yearly_press_interp_mv_avg %<>%
  mutate(
    category = ifelse(parameter == "phosphore_total", "phosphore", category)
  )
# Matières organiques
yearly_press_interp_mv_avg %<>%
  mutate(
    category = ifelse(parameter == "dbo5", "DBO", category),
    category = ifelse(parameter == "oxygene_dissous", "disolved_oxygen", category)
  )

yearly_press_interp_mv_avg_cleaned <- yearly_press_interp_mv_avg %>%
  select(id, year, parameter, value_corrected, direction, category, ld50_fish) %>%
  rename(press = value_corrected)

press_metrics <- gather_press_to_category(
  press = yearly_press_interp_mv_avg_cleaned,
  polluant_units = polluant_units,
  var_to_sum = c("press"),
group_var = c("id", "category", "year"))

#Add temperature and flow
myload(yearly_flow_press_interp_mv_avg, dir = mypath("data-raw", "flow"))
myload(yearly_temp_press_interp_mv_avg, dir = mypath("data-raw", "naiades_temperatures"))
temp_flow_temp <- yearly_flow_press_interp_mv_avg %>%
  mutate(category = "flow") %>%
  bind_rows(mutate(yearly_temp_press_interp_mv_avg, category = "temperature")) %>%
  select(id, year, category,value_corrected) %>%
  rename(press = value_corrected)

temp_flow_temp %<>%
  filter(id %in% year_station$station) %>%
  group_by(id) %>%
  nest() %>%
  left_join(rename(year_station, id = station), by = "id") %>%
  mutate(sorted = map2(data, year_list, function (x, year_list) {
      filter(x, year %in% c(min(year_list) - 1, year_list))
})) %>%
  select(-data, -year_list) %>%
  unnest(sorted)

press_metrics %<>% bind_rows(temp_flow_temp)

temporal_press_polluants <- press_metrics %>%
  group_by(id, category) %>%
  summarise(
    press_med = median(press, na.rm = TRUE),
    cv_press = sd(press, na.rm = TRUE) / mean(press, na.rm = TRUE),
    press = mean(press, na.rm = TRUE))

mysave(yearly_press_interp_mv_avg_cleaned,
  temporal_press_polluants,
  press_metrics,
  dir = mypath("data"), overwrite = TRUE)


myload(temporal_press_polluants, dir = mypath("data"))

debug(gather_press_to_category)
gather_press_to_category(press = test,
  polluant_units = polluant_units,
  var_to_sum = "press",
group_var = c("id", "category", "year"))


######################################
#  Z-press category for press 10_90  #
######################################

myload(press1090_polluants, dir = mypath("data"))
myload(press_cat, dir = mypath("data-raw", "polluants"))
press <- press1090_polluants %>%
  left_join(press_cat, by = "parameter")

# Scale:
press %<>%
  group_by(parameter) %>%
  mutate_at(vars(matches("press")), scale)

# Define press10 for decreasing gradient and vice et versa:
press %<>%
  mutate(z_press =
    pmap_dbl(list(low = press10, high = press90, dire = direction),
      function (dire, low, high) {
	if (dire == "decreasing") {
	  return(low * -1)
	} else{
	  return(high)
	}
      }
      )
  )
# Sum Z score for each category:
press %<>%
  group_by(id, category) %>%
  summarise(z_sum = sum(z_press))

press1090_z_category <- ungroup(press)
mysave(press1090_z_category, dir = mypath("data"), overwrite = TRUE)
