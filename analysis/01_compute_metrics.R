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

######################
#  Community metric  #
######################
library(vegan)
# Caution: Here the richness of the community is different from the richness of
# the network beacause we do not have basal nodes.
#

# Get station and  
data(length_analysis)
data(op_analysis)
op_analysis %<>%
  select(opcod, station, year)
length_analysis %<>%
  group_by(opcod, species) %>%
  summarise(nind = n()) %>%
  left_join(., op_analysis, by = "opcod")
com <- length_analysis %>%
  group_by(station) %>%
  select(-opcod) %>%
  nest()

com %<>%
  mutate(
    com = map2(data, station, function(x, y) {
      message(sprintf('Station %s', y))
      com_matrix <- x %>%
    rowid_to_column() %>%
    spread(species, nind) %>%
    select(-rowid) %>%
    group_by(year) %>%
    summarise_all(funs(sum(., na.rm =TRUE))) %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
  com_matrix
}
    )
  )

# Compute beta-diversity 

# Here I take the overall mean of differences between years but could be an idea
# to consider the mean of dissimilarity year to year (i.e. the diagonal values)
com %<>%
  mutate(
    betadiv = map_dbl(com, function(com){
      vegdist(com, method = "bray", binary = FALSE) %>% mean
    })
  )
# Compute mean and cv of richness
com$richness <- map(com$data, function(x){
  x %<>% group_by(year) %>%
    summarise(richness = n())

  list(mean = mean(x$richness), cv = mean(x$richness) / sd(x$richness))
  })
com %<>% mutate(
  mean_richness = map_dbl(richness, "mean"),
  cv_richness = map_dbl(richness, "cv")
) %>%
  select(-com, -richness)

community_metrics <- com
devtools::use_data(community_metrics, overwrite = TRUE)
