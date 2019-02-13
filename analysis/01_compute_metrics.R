################################################################################
#                           Compute network metrics                            #
################################################################################


# Dep 
library(tidyverse)
library(magrittr)
library(igraph)
devtools::load_all()

#####################
#  Network metrics  #
#####################

library(NetIndices)
library(furrr)
library(tictoc)

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
network_analysis %<>%
  mutate(
    connectance = map_dbl(metrics, "C"),
    richness = map_dbl(metrics, "N"),
    compartiment = map_dbl(metrics, "Cbar"),
    troph_level = future_map(network, NetIndices::TrophInd),
    troph_level2 = map(troph_level, "TL"),
    troph_length = map_dbl(troph_level2, max)
  )

network_analysis[1,]$troph_level2[[1]]
network_metrics <- network_analysis %<>%
  dplyr::select(opcod, connectance, richness, compartiment, troph_length)

devtools::use_data(network_metrics, overwrite = TRUE)
rm(list = ls())

#############
#  Biomass  #
#############
wl <- read_delim("../data-raw/weight_length_coef.csv",
  delim = ";",
  local = locale(decimal_mark = "."),
  col_types = "cddc"
)
wl %<>% dplyr::select(species_code, a, b) %>%
  rename(species = species_code)
data(length_analysis)

weight_analysis <- length_analysis %>%
  left_join(., wl, by = "species") %>%
  mutate(
    weight = a * (length ^ b), #in miligrams
    weight = weight * 10 ^ -3 #in grams
  ) %>%
  dplyr::select(opcod, species, length, weight)
# length is given in milimeters

devtools::use_data(weight_analysis, overwrite = TRUE)
