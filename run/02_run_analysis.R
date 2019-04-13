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
# Make network of species:

# Path to save network analysis:
mypath <- rprojroot::find_package_root_file
if (!is.null(options("network.type") & options("network.type") = "species") {
  dest_dir <- mypath("data", "species")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
} else {
  dest_dir <- mypath("data", "classes")

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
}
data_common <- mypath("data")

cat("Working directory:\n")
cat(getwd(), "\n")

source("../R/misc.R")

source(mypath("analysis", "00_get_network.R"))
cat("Got networks!\n")

 
source(mypath("analysis", "01_compute_community_metrics.R"))
cat("Got community metrics!\n")

source(mypath("analysis", "02_compute_network_metrics.R"))
cat("Got network metrics!\n")
 
source(mypath("analysis", "03_compute_temporal_network_properties.R"))
cat("Got temporal network metrics!\n")

#cat("Compile results\n")
#rmarkdown::render(mypath("vignettes", "results.Rmd"))
