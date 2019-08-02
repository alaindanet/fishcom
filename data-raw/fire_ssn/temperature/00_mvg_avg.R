################################################################################
#                              Prepare temp data                               #
################################################################################


library(tidyverse)
library(magrittr)
library(lubridate)
library('zoo') 
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
myload(quality_data, dir = mypath("data-raw"))

unique(quality_data$validity)
prep_data <- quality_data %>%
  filter(! validity %in% c("9", "Correcte")) %>%
  filter(var_code == "temp") %>%
  mutate(
    value = as.numeric(value),
    year_month = ymd(paste0(year(meas_date),"-", month(meas_date), "-15"))
    ) %>%
  group_by(id, year_month) %>%
  summarise(value = mean(value))

# Eliminate station with few records:
few_records <- prep_data %>%
  group_by(id) %>%
  summarise(nobs = n()) %>%
  filter(nobs < 24) 

prep_data %<>%
  filter(! id %in% few_records$id) 

# Additional data check 
prep_data %<>%
  filter(!value > 100, !value < -4)


prep_data %<>%
  group_by(id) %>%
  arrange(desc(year_month)) %>%
  nest()

rm(quality_data)
options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
    rollapplyr(data = x$value, width = 12, FUN = mean, na.rm = TRUE, fill = NA, partial = 6)
    })
prep_data %<>%
  unnest()
prep_data %<>%
  filter(year_month > "1994-01-01")

monthly_avg_temp <- prep_data

# Yearly avg
yearly_avg_temp <- monthly_avg_temp %>%
  mutate(year = year(year_month)) %>%
  group_by(id, year) %>%
  summarise(value = mean(moving_avg, na.rm = TRUE), raw_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# To play:
sample_monthly_avg_temp <- monthly_avg_temp %>%
  filter(id %in% sample(monthly_avg_temp$id, 100))

mysave(sample_monthly_avg_temp, monthly_avg_temp, yearly_avg_temp,
  dir = mypath("data-raw", "temp"), overwrite = TRUE)

################################################################################
#                                Define pulses                                 #
################################################################################
#TODO: define low and high pulse for temp data


myload(monthly_avg_temp, dir = mypath("data-raw", "temp"))
mtemp <- monthly_avg_temp

#################
#  Local pulse  #
#################
# detrend data
mtemp %<>%
  mutate(value = value - moving_avg)
# define pulse by station

treshold <- 0.01
quant_station <- mtemp %>%
  group_by(id) %>%
  summarise(high_tresh = quantile(value, probs = 1 - treshold, na.rm = TRUE),
  low_tresh = quantile(value, probs = treshold, na.rm = TRUE)
  )

pulse <- mtemp %>%
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
  mutate(year = year(year_month)) %>%
  group_by(id, year) %>%
  summarise(
    nb_low_pulse = sum(low_pulse, na.rm = TRUE),
    nb_high_pulse = sum(high_pulse, na.rm = TRUE),
    low_pulse = any(low_pulse, na.rm = TRUE),
    high_pulse = any(high_pulse, na.rm = TRUE)
  )
local_yearly_pulse_temp <- yearly_pulse %>%
  ungroup()
summary(local_yearly_pulse_temp$nb_low_pulse)


mysave(local_yearly_pulse_temp, dir = mypath("data-raw", "temp"), overwrite = TRUE)

####################
#  National pulse  #
####################

treshold <- 1.96 #95%
pulse <- mtemp %>%
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
  mutate(year = year(year_month)) %>%
  group_by(id, parameter, year) %>%
  summarise(
    nb_pulse = sum(pulse, na.rm = TRUE),
    pulse = any(pulse, na.rm = TRUE)
  )
global_yearly_pulse_temp <- yearly_pulse %>%
  ungroup()

mysave(global_yearly_pulse_temp, dir = mypath("data-raw", "temp"))

###################################
#  Summary data to filter interp  #
###################################

myload(monthly_avg_temp, yearly_avg_temp,
  dir = mypath("data-raw", "temp"))

dist_yearly_avg_temp <- yearly_avg_temp %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

dist_monthly_avg_temp <- monthly_avg_temp %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

mysave(dist_monthly_avg_temp, dist_yearly_avg_temp,
  dir = mypath("data-raw", "temp"), overwrite = TRUE)
