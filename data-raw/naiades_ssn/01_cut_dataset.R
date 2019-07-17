################################################################################
#                          Cut the dataset in pieces                           #
################################################################################

library(tidyverse)
library('lubridate')
library(magrittr)
library(stringr)
library(stringi)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))
myload(yearly_avg_polluants, dir = mypath("data-raw", "polluants"))

replace_rules <- c(
  "\\(" = "-",
  "\\)" = "-",
  "\\'" = ""
)
yearly_avg_polluants %<>%
  mutate(
    parameter = stri_trans_general(parameter, "Latin-ASCII"),
    parameter = tolower(parameter),
    parameter = str_replace_all(parameter, " ", "_"),
    parameter = str_replace_all(parameter, replace_rules)
    )
saving_dir <- mypath("data-raw", "polluants", "yearly_parameter_data")
length(unique(yearly_avg_polluants$parameter))
if (!dir.exists(saving_dir)) {
  dir.create(saving_dir)
}

# Filter data according to the station selected in 00_basin_ssn.R
myload(station_to_keep, dir = mypath("data-raw", "naiades_ssn"))
length(unique(yearly_avg_polluants$id))
length(unique(station_to_keep$id))
yearly_avg_polluants %<>%
  filter(id %in% station_to_keep$id)

yearly_avg_polluants %>%
  group_by(parameter) %>%
  nest() %>%
  mutate(
    saving = purrr::map2(parameter, data, function(parameter, data){
      save(data, file = paste0(saving_dir, "/", parameter, ".rda"),
      compress = "bzip2")
      invisible()
}))

mysave(yearly_avg_polluants, dir = mypath("data-raw", "polluants"), overwrite = TRUE)

