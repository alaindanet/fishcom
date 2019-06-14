
library('tidyverse')
library('magrittr')

#  Interpolation
myload(quality_data, dir = mypath("data-raw"))
library('SSN')
interpolate_basin(ssn_dir = mypath("data-raw", "ssn_interpolation"),
  basin_name = "nord", quality_data = quality_data, var = c("NH4", "NO2", "NO3", "PO4"))
