#######################
#  Interpolate flow   #
#######################

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

# Get flow avg by donuts station:
myload(flow_data, dir = mypath("data-raw"))
flow_avg_complete <- prepare_data_interpolation(data = flow_data,
  date = meas_date, var = value, donuts = donuts, id = id)

# The best correlation structure is: LinearSill.tailup + Mariah.taildown
test <- interpolate_ssn(
  ssn = ssn,
  data = filter(flow_avg_complete, year == 1998),
  group = year, var = avg_data)
