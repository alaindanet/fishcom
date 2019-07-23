#!/usr/bin/env Rscript
# Args should be to specify the type of cluster you are on or at home in order
# to find the cpus
# See: ?availableCores
args <- commandArgs(trailingOnly=TRUE)
if (!is.null(args) & length(args) == 2) {
  options(future.availableCores.methods = args[1])
  message("The first argument has been passed to the
    future.availableCores.methods option.
    On the cluster, it should be 'PBS'.")
  options("network.type" = args[2])
  message("The second argument has been passed to the
    network.type option. It should be species for species network.
    Other values create size class network.")
  
} else if (!is.null(args) & length(args) == 1) {
  options(future.availableCores.methods = args[1])
  message("The first argument has been passed to the
    future.availableCores.methods option.
    On the cluster, it should be 'PBS'.")
}
options(echo = TRUE)

# Path to save network analysis:
mypath <- rprojroot::find_package_root_file

source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))


# Make moving windows on raw data:
source(mypath("data-raw", "polluants", "03_moving_windows.R")

# Make press interpolation: 
interp_scripts <- c(
  "00_basin_ssn.R",
  "00bis_save_ssn.R",
  "01_cut_dataset.R",
  "02_basin_interpolation.R",
  "03_inspect_results.R"
      )
paths <- paste0(mypath("data-raw", "polluants/"), interp_scripts)
for (i in seq_along(interp_scripts)) {
  source(paths[i])
}
