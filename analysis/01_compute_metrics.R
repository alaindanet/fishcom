################################################################################
#                           Compute network metrics                            #
################################################################################


# Dep 
library(tidyverse)
library(magrittr)
library(igraph)
library(NetIndices)
library(furrr)
library(tictoc)
devtools::load_all()

data(network_analysis)

tic()
plan(multiprocess)
network_analysis %<>%
  mutate(
    network = future_map(network, igraph::graph_from_data_frame, directed = TRUE),
    network = future_map(network, igraph::as_adjacency_matrix, sparse = FALSE),
    metrics = future_map(network, NetIndices::GenInd)
    )
toc()
#with parallel: 158 sec
#without parallel: 217 sec
temp_network_analysis <- network_analysis
devtools::use_data(temp_network_analysis, overwrite = TRUE)
