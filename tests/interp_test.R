###############################################
#  Test basin interpolation with north basin  #
###############################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

#  Interpolation
library('SSN')
myload(quality_data, dir = mypath("data-raw"))

var_chr <- c("CONDUCTY")
quality_data %<>% filter(var_code %in% var_chr)

interpolate_basin(ssn_dir = mypath("data-raw", "ssn_interpolation"),
  basin_name = "nord", quality_data = quality_data,
  var = var_chr,#"TN", "TP", "SO4", "temp", "PHY_CHLA", "D_OXY",
  complete = TRUE
)
