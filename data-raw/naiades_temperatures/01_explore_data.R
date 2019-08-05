################################################################################
#                        Explore naiades chemestry data                        #
################################################################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data <- read_delim(mypath("data-raw", "naiades_temperatures", "analyse.csv"),
  delim = ";", locale = locale(date_names = "fr"))
replacement_col <- c(
  "CdStationMesureEauxSurface" = "id",
  "DtAnaTemp" = "date",
  "HrAnaTemp" = "time",
  "RsAnaTemp" = "value"
)

data %<>% .[, names(replacement_col)]
colnames(data) %<>% str_replace_all(., replacement_col)

# Filter abberant temperature:
check <- data %>%
  group_by(id) %>%
  mutate(
    check_up = value > mean(value) + 5 * sd(value),
    check_dw = value < mean(value) - 5 * sd(value)
  )

# Correct for abberant date:
check %<>%
  mutate(date = as.character(date))
## Date without "20" for the year:
check %<>%
  mutate(
    date = ifelse(str_length(date) == 8, str_c("20", date), date)
  )

## Date with typo for the year:
check %<>%
  mutate(
    date = ifelse(!is.na(str_match(date, "2045.*")), str_c("2015", str_sub(date, start = 5)), date),
    date = ifelse(!is.na(str_match(date, "2046")), str_c("2016", str_sub(date, start = 5)), date)
  )
check %<>%
  mutate(date = ymd(date))

check %<>%
  mutate(value = ifelse(check_up | check_dw, NA, value)) %>%
  select(-check_up, -check_dw)


hourly_temp_naiades <- check
sample_hourly_temp_naiades <- filter(check, id %in% sample(unique(check$id), 50))
mysave(hourly_temp_naiades, dir = mypath("data-raw", "naiades_temperatures"), overwrite = TRUE)
mysave(sample_hourly_temp_naiades, dir = mypath("data-raw", "naiades_temperatures"), overwrite = TRUE)
