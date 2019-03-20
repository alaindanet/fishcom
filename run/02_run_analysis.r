#!/usr/bin/env Rscript
# Args should be to specify the type of cluster you are on or at home in order
# to find the cpus
# See: ?availableCores
args <- commandArgs(trailingOnly=TRUE)
if (!is.null(args) & length(args) > 1) {
  stop("Only one argument should be specified")
} else if (!is.null(args) & is.character(args)) {
  options(future.availableCores.methods = args[1])
}
options(echo = TRUE)
options("network.type" = "species")

cat("Working directory:\n")
cat(getwd(), "\n")

source("../analysis/00_get_network.R")
cat("Got networks!\n")
 
source("../analysis/01_compute_community_metrics.R")
cat("Got community metrics!\n")

source("../analysis/02_compute_network_metrics.R")
cat("Got network metrics!\n")
 
source("../analysis/03_compute_temporal_network_properties.R")
cat("Got temporal network metrics!\n")

cat("This is a perfect!")
