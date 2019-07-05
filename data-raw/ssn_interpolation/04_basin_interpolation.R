
library(tidyverse)
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

#  Interpolation
library('SSN')
library(parallel)
basin <- c("ouest", "sud", "est", "nord") #"nord"
myload(quality_data, dir = mypath("data-raw"))
options(mc.cores = 5)
mclapply(basin, function (x) {
  interpolate_basin(ssn_dir = mypath("data-raw", "ssn_interpolation"),
    basin_name = x, quality_data = quality_data,
    var = c("TN", "TP", "CONDUCTY", "SO4", "temp", "PHY_CHLA", "D_OXY"),
    complete = TRUE
  )
})
