################################################################################
#                     Exploration of interpolation results                     #
################################################################################

library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

# Load fish and environmental station
myload(quality_prediction, dir = mypath("data-raw", "ssn_interpolation", "nord"))

cross_val <- quality_prediction %>%
  select(var_code, cross_v)
cross_val$cross_v[[1]]

