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
data <- do.call(rbind, test)

data %<>%
  filter(date > "1994-01-01") %>%
  select(id, date, time, parameter, value) %>%
  mutate(value = as.numeric(value))
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


