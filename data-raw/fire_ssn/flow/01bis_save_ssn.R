library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))


myload(my_hydro_basin, dir = mypath("data-raw", "fire_ssn"))

library(parallel)
ssn <- mclapply(my_hydro_basin$basin_name, function(basin) {
  pred_name <- paste0(basin, "_pred_sites")
  ssn_dir <- mypath("data-raw", "fire_ssn", "flow")
  ssn_obj_path <- paste0(ssn_dir, "/", basin, ".ssn")
  ssn <- SSN::importSSN(ssn_obj_path, predpts = pred_name, o.write = TRUE)
# Compute the weight of each streams lines when they merged:
  ssn <- SSN::additive.function(ssn, "H2OArea",
    "afv_area")
# create distance matrix between pred and obs:
  SSN::createDistMat(ssn, predpts = pred_name, o.write = TRUE,
    amongpreds = TRUE)
  #SSN::writeSSN(ssn, filename = ssn_obj_path, o.write = TRUE)
  return(ssn)
})
names(ssn) <- my_hydro_basin$basin_name 
mysave(ssn, dir = mypath("data-raw", "fire_ssn", "flow"), overwrite = TRUE)
