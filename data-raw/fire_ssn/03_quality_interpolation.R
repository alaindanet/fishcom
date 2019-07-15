################################################################################
#                             Interpolate quality                              #
################################################################################

mypath <- rprojroot::find_package_root_file
Sys.setenv(LANG = "en")
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))
library(SSN)
library(tidyverse)
library(magrittr)

ssn_dir <- mypath("data-raw", "ssn_interpolation", "donuts_station.ssn")
ssn <- importSSN(ssn_dir, predpts = "station", o.write = TRUE)
names(ssn)

# Compute the weight of each streams lines when they merged: 
ssn <- additive.function(ssn, "H2OArea",
  "afv_area")

# create distance matrix between pred and obs:
createDistMat(ssn,
  predpts = "station", o.write = TRUE, amongpreds = TRUE)

# Get quality yearly avg by donuts station:
myload(quality_data, dir = mypath("data-raw"))
myload(donuts_analysis, dir = mypath("data"))
donuts <- donuts_analysis

# Begin with nitrogen and phosphorus
quality_data %<>%
  filter(var_code %in% c("NH4", "NO2", "NO3", "PO4"))

quality_data %<>%
  group_by(var_code) %>%
  nest()

# Enable parallel computation:
source(mypath("analysis", "misc", "parallel_setup.R"))

# Prepare data
donuts %<>%
  mutate(id = as.character(id))
quality_data %<>%
  mutate(interp_data = furrr::future_map(data,
      ~prepare_data_interpolation(data = .x, date = meas_date, var = value,
	donuts = donuts, id = id, cutoff_day = NULL)))
# Filter data
quality_data %<>%
  select(var_code, interp_data)
# Interpolate
quality_prediction <- quality_data %>%
  group_by(var_code) %>%
  mutate(interp_data = purrr::map(interp_data,
      ~interpolate_ssn(data = .x, ssn = ssn, var = avg_data,
	group = year)))

quality_prediction %<>%
  select(var_code, cross_v, prediction)
mysave(quality_prediction, dir = mypath("data-raw", "ssn_interpolation"))
