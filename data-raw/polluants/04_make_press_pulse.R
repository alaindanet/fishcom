################################################################################
#                             Make press and pulse                             #
################################################################################

library(tidyverse)
library(magrittr)
library('stringr')
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

###########
#  press  #
###########

myload(yearly_press_interp_mv_avg, dir = mypath("data-raw", "polluants"))


press <- yearly_press_interp_mv_avg %>%
  group_by(parameter, id) %>%
  summarise(frac_obs = sum(!is.na(value_corrected)) / n(), press = mean(value_corrected, na.rm = TRUE))

# Keep only the data where we have enough observation 
press %<>%
  filter(frac_obs > 0.75)

press_polluants <- press
mysave(press_polluants, dir = mypath("data"), overwrite = TRUE)


###############################
#  Z-score by press category  #
###############################
myload(press_cat, polluant_units, dir = mypath("data-raw", "polluants"))
myload(press_polluants, dir = mypath("data"))



press_polluants %<>%
  ungroup() %>%
  mutate(parameter = as.character(parameter)) %>%
  left_join(press_cat, by = "parameter")
missing_cat <- filter(press_polluants, is.na(category))
stopifnot(nrow(missing_cat) == 0)

press_polluants %>%
  group_by(category) %>%
  summarise(frac_ld = sum(!is.na(ld50_fish)) / n())

# Polluants avg by those categories and weighted by their ld_50:
ld_cat <- c("herbicides", "insecticides", "fungicides", "pcb", "hap", "micropolluants mineraux", "micropolluants organiques")

# Weighted sum of ld by station:
press_ld <-  press_polluants %>%
  filter(category %in% ld_cat & !is.na(ld50_fish)) %>%
  group_by(id, category) %>%
  summarise(
    press = sum(press * (1 / ld50_fish / sum(1 / ld50_fish, na.rm = TRUE)), na.rm = TRUE)
  ) %>%
  group_by(category) %>%
  mutate(press = scale(press))

# Non ld scale them:
press_non_ld <- press_polluants %>%
  filter(!category %in% ld_cat) %>%
  group_by(parameter) %>%
  mutate(z_press_temp = scale(press)) %>%
  mutate(z_press = map2_dbl(direction, z_press_temp,
      function(direction, z){# take care of the direction of the gradient
	if (direction == "decreasing") {
	  z <- z * -1
	  return(z)
	} else {
	  return(z)
	}
    }))
test_inversion <- filter(press_non_ld, parameter == "ph")
stopifnot(test_inversion[1,]$z_press_temp == test_inversion[1,]$z_press * -1)

press_non_ld %<>%
  select(parameter, id, press, category, z_press)

press_non_ld %<>%
  group_by(id, category) %>%
  summarise(press = sum(z_press))

#Merge the two:
press <- rbind(press_ld, press_non_ld) %>%
  as_tibble() %>%
  unnest()

mysave(press, dir = mypath("data"), overwrite = TRUE)

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
