################################################################################
#                  Filter data according to Olivier Dezerald                   #
################################################################################

library(TSA)
library(tidyverse)
library(magrittr)
library(lubridate)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

analysis_files <- list.files(
  path = mypath("data-raw", "polluants", "naiades_data"),
  pattern = "analyses.*.rda", full.names = FALSE)
pressure_info <- read_csv2(mypath("data-raw", "polluants", "naiades_data", "pressure_info_olivier.csv")) 

library(parallel)
options(mc.cores = 8)
test <- mclapply(analysis_files, function (files) {

  obj_name <- str_sub(files, end = -5) 
  print(obj_name)
  load(mypath("data-raw", "polluants", "naiades_data", files))
  data <- get(obj_name)
  data
  })
temp_data <- test
data <- do.call(rbind, temp_data)

data %<>%
  filter(date > "1994-01-01") %>%
  select(id, date, parameter, value, units) %>%
  mutate(value = as.numeric(value))

#####################
#  Harmonize units  #
#####################
units <- data %>%
  group_by(parameter) %>%
  summarise(n_units = length(unique(units)), units = list(unique(units)))
units %<>%
  unnest()

filter(units, n_units >1)

# Very few data (11) does not have units, let's remove them
data %<>%
  filter(units != "X")

units <- data %>%
  group_by(parameter) %>%
  summarise(n_units = length(unique(units)), units = list(unique(units))) %>%
  unnest()
filter(units, n_units >1)

# We can see that some units contain the name of the molecule in it: 
unique(units$units)
# also
filter(units, units %in% c("°f", "°F"))

# We remove the parenthesis and the name of the molecule inside:
# Try:
str_replace(unique(units$units), "g\\(.*\\)", "g")
str_replace(unique(units$units), "°f", "°F")
# do it:
data %<>%
  mutate(
    units = str_replace(units, "g\\(.*\\)", "g"),
    units = str_replace(units, "°f", "°F")
  )
# Manage the ones that have several units:
units <- data %>%
  group_by(parameter) %>%
  summarise(n_units = length(unique(units)), units = list(unique(units))) %>%
  unnest()
filter(units, n_units >3)
n_by_units <- data %>%
  group_by(parameter, units) %>%
  summarise(n = n())
test <- left_join(n_by_units, units) %>%
  filter(n_units > 1)
## 4 units, tx de saturation en O2:
filter(test, n_units > 3)
### Normalement exprimé en %, suppr the other:
data %<>%
  filter(!(parameter == "Taux de saturation en O2" & units != "%"))

## 3 units: several parameters have weird units 
filter(test, n_units == 3)
###  Remove strange units
data %<>%
  filter(!(parameter == "pH" & units != "unité pH")) %>%
  filter(!(parameter == "Oxygène dissous" & units != "mg/L")) %>%
  filter(!(parameter == "DBO5" & units != "mg/L")) %>%
  filter(!(parameter == "Carbone Organique" & units != "mg/L")) %>%
  filter(!(parameter == "Turbidité Néphélométrique" & units != "NFU"))

units <- data %>%
  group_by(parameter) %>%
  summarise(n_units = length(unique(units)), units = list(unique(units))) %>%
  unnest()
n_by_units <- data %>%
  group_by(parameter, units) %>%
  summarise(n = n())
test <- left_join(n_by_units, units) %>%
  filter(n_units > 1)
filter(test, parameter == "Turbidité Néphélométrique")
# Convert units from the less used to the more used: 
most_used_unit <- test %>%
  group_by(parameter) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  select(parameter, units) %>%
  rename(most_used_u = units)
# Get the msot used units per parameter:
data %<>%
  left_join(most_used_unit, by = "parameter")
# Transform units
unique(most_used_unit$most_used_u)
data %<>%
  mutate(value_fixed = pmap_dbl(list(units, most_used_u, value), function(u, most_u, val) {
      if (u == most_u | is.na(most_u)) {
	return(val)
      } else if (u == "ng/L" & most_u == "µg/L") {
	return(val * 10^-3)
      } else if (u == "mg/L" & most_u == "µg/L") {
	return(val * 10^3)
      } else if (u == "µg/L" & most_u == "mg/L") {
	return(val * 10^-3)
      } else if (u == "ng/L" & most_u == "mg/L") {
	return(val * 10^-6)
      } else if (u == "mg/kg" & most_u == "mg/L") {
	return(val)
      } else {
	stop(paste("u =", u, ", most_u = ", most_u, ". Case not handled"))
      }
  }))

filter(data, most_used_u != units, units == "mg/L")

data %<>%
  mutate(
    units = ifelse(is.na(most_used_u), units, most_used_u),
    value = value_fixed) %>%
  select(id, date, parameter, value)

analysis_total <- data

mysave(analysis_total, dir = mypath("data-raw", "polluants", "naiades_data"), overwrite = TRUE)

fourier_data <- data %>%
  ungroup() %>%
  mutate(
    value = as.numeric(value),
    year_month = ymd(paste0(year(date),"-", month(date), "-15"))
    ) %>%
  group_by(parameter, year_month) %>%
  summarise(value = mean(value)) %>%
  group_by(parameter) %>%
  arrange(year_month)  %>%
  summarise(n_month = n(),
    fourier = list(TSA::periodogram(value)))

fourier <- fourier_data %>%
  mutate(
    freq_spec = map(fourier,
      function(x) tibble(freq = x$freq, spec = x$spec)),
    freq_spec = map(freq_spec, ~arrange(., desc(spec))),
    top_freq = map_dbl(freq_spec, ~.[1, ][["freq"]]) # Extract the highest spectral values
    )
test <- TSA::periodogram(c(rnorm(100), NA, rnorm(100)))

fourier %<>%
  mutate(period_month = 1/top_freq) %>%
  filter(!n_month < period_month +10)
fourier %>%
  filter(period_month < 14)
fourier %<>%
  select(-freq_spec)
mysave(fourier, dir = mypath("data-raw", "polluants"), overwrite = TRUE)

