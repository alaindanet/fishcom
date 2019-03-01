#!/usr/bin/env r

source("../analysis/00_get_network.R")
cat("Got networks!\n")
 
source("../analysis/01_compute_community_metrics.R")
cat("Got community metrics!\n")

source("../analysis/02_compute_network_metrics.R")
cat("Got network metrics!\n")
 
source("../analysis/03_compute_temporal_network_properties.R")
cat("Got temporal network metrics!\n")

cat("This is a perfect!")
