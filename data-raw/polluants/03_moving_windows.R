################################################################################
#                     Make moving windows to detrends data                     #
################################################################################


library(tidyverse)
library(magrittr)
library(lubridate)
library(stringi)
library('zoo') 
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
myload(analysis_total, dir = mypath("data-raw", "polluants", "naiades_data"))

prep_data <- analysis_total %>%
  mutate(
    value = as.numeric(value),
    year_month = ymd(paste0(year(date),"-", month(date), "-15"))
    ) %>%
  group_by(parameter, id, year_month) %>%
  summarise(value = mean(value))

# Eliminate station with few records:
few_records <- prep_data %>%
  group_by(id, parameter) %>%
  summarise(nobs = n()) %>%
  filter(nobs < 24) 

prep_data %<>%
  group_by(id, parameter) %>%
  arrange(desc(year_month)) 

prep_data %<>%
  nest()

rm(analysis_total)
options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
    rollapplyr(data = x$value, width = 12, FUN = mean, na.rm = TRUE, fill = NA, partial = 9)
    })
prep_data %<>%
  unnest()

monthly_avg_polluants <- prep_data
replace_rules <- c(
  "\\(" = "-",
  "\\)" = "-",
  "\\'" = ""
)
monthly_avg_polluants %<>%
  mutate(
    parameter = stri_trans_general(parameter, "Latin-ASCII"),
    parameter = tolower(parameter),
    parameter = str_replace_all(parameter, " ", "_"),
    parameter = str_replace_all(parameter, replace_rules)
    )
# Yearly avg
mysave(monthly_avg_polluants,
  dir = mypath("data-raw", "polluants"),
  overwrite = TRUE)

yearly_avg_polluants <- monthly_avg_polluants %>%
  mutate(year = year(year_month)) %>%
  filter(!year %in% c(1994, 2018)) %>% # To epure moving avg NA
  group_by(id, parameter, year) %>%
  summarise(value = mean(moving_avg, na.rm = TRUE), raw_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# To play:
sample_monthly_avg_polluants <- monthly_avg_polluants %>%
  filter(id %in% sample(monthly_avg_polluants$id, 100))

mysave(sample_monthly_avg_polluants, monthly_avg_polluants, yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)

################################################################################
#                                Define pulses                                 #
################################################################################


myload(monthly_avg_polluants,
  press_cat, dir = mypath("data-raw", "polluants"))
mpolluants <- monthly_avg_polluants

mpolluants %<>% left_join(press_cat, by = "parameter")
missing_cat <- filter(mpolluants, is.na(category))
unique(missing_cat$parameter)

#################
#  Local pulse  #
#################
# detrend data
mpolluants %<>%
  mutate(value = value - moving_avg)
# define pulse by station

treshold <- 1.96 #95%
pulse <- mpolluants %>%
  group_by(id, parameter) %>%
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
local_yearly_pulse_polluants <- yearly_pulse %>%
  ungroup()

mysave(local_yearly_pulse_polluants, dir = mypath("data-raw", "polluants"), overwrite = TRUE)

####################
#  National pulse  #
####################

treshold <- 1.96 #95%
pulse <- mpolluants %>%
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
global_yearly_pulse_polluants <- yearly_pulse %>%
  ungroup()

mysave(global_yearly_pulse_polluants, dir = mypath("data-raw", "polluants"))

###################################
#  Summary data to filter interp  #
###################################

myload(monthly_avg_polluants, yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"))

dist_yearly_avg_polluants <- yearly_avg_polluants %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

dist_monthly_avg_polluants <- monthly_avg_polluants %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE))

mysave(dist_monthly_avg_polluants, dist_yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)
