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
options(mc.cores = 3)
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

# Compute nestedness et modularity: 
network_analysis %<>%
  mutate(
  nestedness = future_map_dbl(network, nestedness),
  modul_guimera = future_map(network, rnetcarto::netcarto)
  )

test <- network_analysis %>%
  mutate(
  is_sym = future_map_lgl(network, function (x) nrow(x) == ncol(x))
  )

network_analysis %<>%
  mutate(
    connectance = map_dbl(metrics, "C"),
    nbnode = map_dbl(metrics, "N"),
    richness = map_dbl(network, function (x) {
      rownames(x) %>%
	get_species %>%
	unique %>%
	unlist %>%
	length
	
}),
    compartiment = map_dbl(metrics, "Cbar"),
    troph_level = future_map(network, NetIndices::TrophInd),
    troph_level = map(troph_level, "TL"),
    troph_level_avg = map_dbl(troph_level, mean),
    troph_length = map_dbl(troph_level, max),
    modularity = map_dbl(modul_guimera, 2),
  )
rownames(network_analysis[1, ]$network[[1]]) %>% get_species %>% unique

network_metrics <- network_analysis %>%
  dplyr::select(-network, -community, -metrics, -troph_level)

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
