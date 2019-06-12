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
unique(quality_data$var_code)

# Begin with nitrogen and phosphorus
quality_data %<>%
  filter(var_code %in% c("NH4", "NO2", "NO3", "PO4"))

quality_data %<>%
  group_by(var_code) %>%
  nest()

# Enable parallel computation:
source(mypath("analysis", "misc", "parallel_setup.R"))

donuts %<>%
  mutate(id = as.character(id))
quality_data %<>%
  mutate(data = furrr::future_map2(data, var_code,
      ~prepare_data_interpolation(data = .x, date = meas_date, var = .y,
	donuts = donuts, id = id)))
quality_prediction <- quality_data %>%
  group_by(var_code) %>%
  mutate(data = furrr::future_map2(data, var_code,
      ~interpolate_ssn(data = .x, ssn = ssn, var = .y,
	donuts = donuts, group = year)))

quality_prediction %<>%
  select(var_code, cross_v, prediction)
mysave(quality_prediction, dir = mypath("data-raw", "ssn_interpolation"))
