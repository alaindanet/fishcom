################################################################################
#                     Make moving windows to detrends data                     #
################################################################################


library(tidyverse)
library(magrittr)
library(lubridate)
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
  filter(nobs <24) 

prep_data %<>%
  group_by(id, parameter) %>%
  arrange(desc(year_month)) %>%
  nest()

rm(analysis_total)
options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
    rollapplyr(data = x$value, width = 12, FUN = mean, na.rm = TRUE, fill = NA, partial = 3)
    })
prep_data %<>%
  unnest()

monthly_avg_polluants <- prep_data
# Yearly avg
yearly_avg_polluants <- prep_data %>%
  mutate(year = year(year_month)) %>%
  group_by(id, parameter, year) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup()
mysave(monthly_avg_polluants, yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)

###################################
#  Summary data to filter interp  #
###################################

myload(monthly_avg_polluants, yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"))

dist_yearly_avg_polluants <- yearly_avg_polluants %>%
  group_by(parameter) %>%
  summarise(mean = mean(value), sd = sd(value), min = min(value), max = max(value))

dist_monthly_avg_polluants <- monthly_avg_polluants %>%
  group_by(parameter) %>%
  summarise(mean = mean(value), sd = sd(value), min = min(value), max = max(value))

mysave(dist_monthly_avg_polluants, dist_yearly_avg_polluants,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)
