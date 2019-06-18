
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
options(mc.cores = 4)
mclapply(basin, function (x) {
  interpolate_basin(ssn_dir = mypath("data-raw", "ssn_interpolation"),
    basin_name = x, quality_data = quality_data, var = c("NH4", "NO2", "NO3", "PO4"))
})
