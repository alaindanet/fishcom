################################################################################
#                              Prepare flow data                               #
################################################################################


library(tidyverse)
library(magrittr)
library(lubridate)
library('zoo') 
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
myload(flow_data, dir = mypath("data-raw"))

prep_data <- flow_data %>%
  mutate(
    value = as.numeric(value),
    ) %>%
  group_by(id, meas_date) %>%
  summarise(value = mean(value))

# Eliminate station with few records:
# Not necessary for flow data bc the nb of station is low.

prep_data %<>%
  group_by(id) %>%
  arrange(meas_date) %>%
  nest()

rm(flow_data)
options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
    rollapplyr(data = x$value, width = 365, FUN = mean, na.rm = TRUE, fill = NA, partial = 120)
    })
prep_data %<>%
  unnest()
prep_data %<>%
  filter(meas_date > "1994-01-01")

daily_avg_flow <- prep_data
# Yearly avg
mysave(daily_avg_flow, dir = mypath("data-raw", "flow"))

yearly_avg_flow <- daily_avg_flow %>%
  mutate(year = year(meas_date)) %>%
  group_by(id, year) %>%
  summarise(value = mean(moving_avg, na.rm = TRUE), raw_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# To play:
sample_daily_avg_flow <- daily_avg_flow %>%
  filter(id %in% sample(daily_avg_flow$id, 100))

mysave(sample_daily_avg_flow, daily_avg_flow, yearly_avg_flow,
  dir = mypath("data-raw", "flow"), overwrite = TRUE)

################################################################################
#                                Define pulses                                 #
################################################################################
#TODO: define low and high pulse for flow data


myload(daily_avg_flow, dir = mypath("data-raw", "flow"))
mflow <- daily_avg_flow

#################
#  Local pulse  #
#################
# detrend data
mflow %<>%
  mutate(value = value - moving_avg)
# define pulse by station

treshold <- 0.01
quant_station <- mflow %>%
  group_by(id) %>%
  summarise(high_tresh = quantile(value, probs = 1 - treshold, na.rm = TRUE),
  low_tresh = quantile(value, probs = treshold, na.rm = TRUE)
  )

pulse <- mflow %>%
  left_join(quant_station) %>%
  group_by(id) %>%
  mutate(
    high_pulse = map2_lgl(value, high_tresh,
      function (z, treshold) {
	if (is.na(z)) return(NA)
	if (z > treshold) return(TRUE) else return(FALSE)
      }
    ),
    low_pulse = map2_lgl(value, low_tresh,
      function (z, treshold) {
	if (is.na(z)) return(NA)
	if (z < treshold) return(TRUE) else return(FALSE)
      }
    )
  )
yearly_pulse <- pulse %>%
  mutate(year = year(meas_date)) %>%
  group_by(id, year) %>%
  summarise(
    nb_low_pulse = sum(low_pulse, na.rm = TRUE),
    nb_high_pulse = sum(high_pulse, na.rm = TRUE),
    low_pulse = any(low_pulse, na.rm = TRUE),
    high_pulse = any(high_pulse, na.rm = TRUE)
  )
local_yearly_pulse_flow <- yearly_pulse %>%
  ungroup()
summary(local_yearly_pulse_flow$nb_high_pulse)

mysave(local_yearly_pulse_flow, dir = mypath("data-raw", "flow"), overwrite = TRUE)

####################
#  National pulse  #
####################

treshold <- 1.96 #95%
pulse <- mflow %>%
  group_by(parameter) %>%
  mutate(
    z = scale(value),
    pulse = map2_lgl(z, direction,
      function (z, dire) {
	if (is.na(z)) return(NA)

	if (dire == "decreasing") {
	  if (z < treshold * -1) {
	    return(TRUE)
	  } else {
	    return(FALSE)
	  }
	} else{
	  if (z > treshold) {
	    return(TRUE)
	  } else {
	    return(FALSE)
	  }
	}
      }
    )
  )
yearly_pulse <- pulse %>%
  mutate(year = year(meas_date)) %>%
  group_by(id, year) %>%
  summarise(
    nb_pulse = sum(pulse, na.rm = TRUE),
    pulse = any(pulse, na.rm = TRUE)
  )
global_yearly_pulse_flow <- yearly_pulse %>%
  ungroup()

mysave(global_yearly_pulse_flow, dir = mypath("data-raw", "flow"))

###################################
#  Summary data to filter interp  #
###################################

myload(daily_avg_flow, yearly_avg_flow,
  dir = mypath("data-raw", "flow"))

dist_yearly_avg_flow <- yearly_avg_flow %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

dist_daily_avg_flow <- daily_avg_flow %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

mysave(dist_daily_avg_flow, dist_yearly_avg_flow,
  dir = mypath("data-raw", "flow"), overwrite = TRUE)
