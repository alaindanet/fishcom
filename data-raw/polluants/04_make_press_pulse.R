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
myload(press_cat, dir = mypath("data-raw", "polluants"))
myload(polluant_units, dir = mypath("data-raw", "polluants", "naiades_data"))


# If several values have been interpolated, keep the safest
press_metrics <- format_press_polluant(
  press = yearly_press_interp_mv_avg,
  polluant_units = polluant_units,
  press_cat = press_cat
)

#Add temperature and flow
myload(yearly_flow_press_interp_mv_avg, dir = mypath("data-raw", "flow"))
myload(yearly_temp_press_interp_mv_avg, dir = mypath("data-raw", "naiades_temperatures"))

flow_temp_metrics <- get_temp_flow_metrics(
  flow = yearly_flow_press_interp_mv_avg,
  temp = yearly_temp_press_interp_mv_avg
)

temporal_press_polluants <- compute_temporal_press(
  press_metrics = press_metrics,
  temp_flow_metrics = flow_temp_metrics,
  .op = op_analysis
)

mysave(temporal_press_polluants, flow_temp_metrics, press_metrics,
  dir = mypath("data"), overwrite = TRUE)


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
