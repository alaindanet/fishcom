################################################################################
#                           Compute network metrics                            #
################################################################################
cat("-----------------------\n")
cat("Compute network metrics\n")
cat("-----------------------\n")


# Dep 
library(tidyverse)
library(stringr)
library(magrittr)
library(igraph)
library(rnetcarto)
library(furrr)
library(tictoc)
#library(ggmisc)
devtools::load_all()


#########################################
#  Add node biomass and node abundance  #
#########################################
cat("-----------------------\n")
cat("Add node biomass and node abundance\n")
cat("-----------------------\n")

# Load
myload(weight_analysis, metaweb_analysis, dir = data_common)
myload(network_analysis, dir = dest_dir)

# Filter data according to the metaweb
weight_analysis <- sanatize_metaweb(
  data = weight_analysis,
  species = species,
  fish_diet_shift = metaweb_analysis$size_class,
  nb_class = max(unique(metaweb_analysis$size_class$class_id))
  )

# compute weight by node and by opcod
weight_analysis %<>% assign_size_class(., species, var = length,
  classes = metaweb_analysis$size_class) %>%
  unite(sp_class, species, class_id, sep = "_") %>%
  dplyr::select(opcod, sp_class, weight)
# Get abundance class
abundance <- weight_analysis %>%
  group_by(opcod, sp_class) %>%
  summarise(nind = n())

# Get biomass by op
myload(op_analysis, dir = data_common)
st_timing <- op_analysis %>%
  dplyr::select(opcod, station, year) %>%
  mutate(times = year)
rm(op_analysis)

# Sum biomass by op
biomass <- left_join(weight_analysis, st_timing, by = "opcod") %>%
  group_by(opcod, station, times) %>%
  nest()

# Associate biomass to network 
biomass_node <- weight_analysis %>%
  group_by(opcod, sp_class) %>%
  summarise(biomass = sum(weight))
network_composition <-
  left_join(abundance, biomass_node, by = c("opcod", "sp_class")) %>%
  group_by(opcod) %>%
  nest(.key = composition)
network_analysis <-
  left_join(network_analysis, network_composition, by = "opcod")


mysave(network_analysis, dir = dest_dir, overwrite = TRUE)

######################################
#  Network biomass by trophic group  #
######################################
cat("-----------------------\n")
cat("Network biomass by trophic group\n")
cat("-----------------------\n")

# Get biomass by trophic level
## Compute trophic level by node
g <- igraph::graph_from_adjacency_matrix(metaweb_analysis$metaweb,
  mode = "directed")
dead_material <- c("det", "biof")
## Compute trophic level by node with metaweb:
trophic_level <- NetIndices::TrophInd(metaweb_analysis$metaweb, Dead = dead_material) %>%
  mutate(sp_class = colnames(metaweb_analysis$metaweb)) %>%
  rename(troph_level = TL) %>%
  select(sp_class, troph_level)
summary(trophic_level)
## Split trophic level in three classes:
trophic_class <- split_in_classes(trophic_level$troph_level, class_method = "percentile",
  nb_class = 3, round_limits = FALSE)
mysave(trophic_class, dir = dest_dir, overwrite = TRUE)

## Assign trophic group to each node  
trophic_level %<>%
  mutate(
    troph_group = get_size_class(trophic_level, NULL, troph_level, trophic_class)
  )

## Assign to each network its trophic group:
source('../analysis/misc/parallel_setup.R')
network_analysis %<>%
  mutate(
  composition = furrr::future_map(composition, function (compo, troph_group){
    left_join(compo, troph_group, by = "sp_class")
}, troph_group = trophic_level))

## Compute trophic level by species:
if (!is.null(options("network.type")) & options("network.type") == "species") {

  composition <-
    network_analysis %>% unnest(composition) %>%
    mutate(species = str_extract(sp_class, "[A-Z]{3}")) %>%
    group_by(opcod, species) %>%
    summarise(
      # Average species trophic level by biomass:
      troph_level = sum(biomass * troph_level) / sum(biomass),
      biomass = sum(biomass)
    ) %>%
    ungroup() %>%
    mutate(
      troph_group = get_size_class(., NULL, troph_level, trophic_class)
    ) %>%
    group_by(opcod) %>%
    nest(.key = composition)

  network_analysis %<>%
    select(-composition) %>%
    left_join(., composition, by = "opcod")

  network_analysis$troph_group <-
    furrr::future_map(network_analysis$composition, function(compo) {
      compo %<>%
	group_by(troph_group) %>%
	summarise(
	  biomass = sum(biomass),
	  nbnode  = n(),
	  richness = n()
	  ) %>%
	ungroup()
    })
} else {
  # for network with classes:
  ## Sum biomass by trophic group:
  ### I do this way bc of a dplyr bug with n() in nested data.frame
  network_analysis$troph_group <-
    furrr::future_map(network_analysis$composition, function(compo) {
      compo %<>%
	group_by(troph_group) %>%
	summarise(
	  biomass = sum(biomass),
	  nbnode  = n(),
	  richness = str_extract_all(sp_class, "[A-Z]") %>% unique %>% length
	  ) %>%
	ungroup()
    })
}

network_analysis %>%
  unnest(troph_group) %>%
  filter(is.na(troph_group))
network_analysis %>%
  unnest(composition) %>%
  filter(opcod == 25770)

mysave(network_analysis, dir = dest_dir, overwrite = TRUE)

#####################
#  Network metrics  #
#####################
cat("-----------------------\n")
cat("Compute network metrics\n")
cat("-----------------------\n")

library(NetIndices)

myload(network_analysis, dir = dest_dir)

source('../analysis/misc/parallel_setup.R')
cat("Convert graph to matrix:\n")
tic()
if (!is.null(options("network.type")) & options("network.type") == "species") {
  # Network from class to species:
  network_analysis %<>%
    mutate(network = future_map(network, function (net){
	net %<>% mutate(
	  from = str_extract(from, "[a-zA-Z]+"),
	  to = str_extract(to, "[a-zA-Z]+")
	  ) %>%
	distinct(from, to)
	  }))
  mysave(network_analysis, dir = dest_dir, overwrite = TRUE)
}
# Transform network to adjacency matrix: 
network_analysis %<>%
  mutate(
    network = future_map(network, igraph::graph_from_data_frame, directed = TRUE),
    network = future_map(network, igraph::as_adjacency_matrix, sparse = FALSE)
    )
# Compute generic indices:
network_analysis %<>%
  mutate(
    metrics = future_map(network, NetIndices::GenInd)
    )
toc()
#with parallel: 158 sec
#without parallel: 217 sec

source('../analysis/misc/parallel_setup.R')
source('../R/nestedness.R')
cat("Compute nestedness et modularity:\n")
network_analysis %<>%
  mutate(
  nestedness = future_map_dbl(network, nestedness),
  modul_guimera = future_map(network, rnetcarto::netcarto)
  )

source('../R/diet_overlap.R')
cat("Compute species diet overlap:\n")
network_analysis %<>%
  mutate(
  diet_overlap = future_map(network, compute_diet_overlap),
  diet_overlap = future_map(diet_overlap, average_species_overlap),
  avg_diet_overlap = future_map_dbl(diet_overlap, mean)
  )

source('../analysis/misc/parallel_setup.R')
cat("Test correctness of network:\n")
test <- network_analysis %>%
  mutate(
  is_sym = future_map_lgl(network, function (x) nrow(x) == ncol(x))
  )

source('../analysis/misc/parallel_setup.R')
cat("Get network indices:\n")
network_analysis %<>%
  mutate(
    connectance = map_dbl(metrics, "C"),
    nbnode = map_dbl(metrics, "N"),
    compartiment = map_dbl(metrics, "Cbar"),
    troph_level = future_map(network, NetIndices::TrophInd),
    troph_level = map(troph_level, "TL"),
    troph_level_avg = map_dbl(troph_level, mean),
    troph_length = map_dbl(troph_level, max),
    modularity = map_dbl(modul_guimera, 2)
  )

network_metrics <- network_analysis %>%
  dplyr::select(-metrics, -troph_level)
mysave(network_metrics, dir = dest_dir, overwrite = TRUE)

##############################
#  Standardized connectance  #
##############################
cat("-----------------------\n")
cat("Standardize connectance\n")
cat("-----------------------\n")

myload(network_metrics, dir = dest_dir)

c_richness_mod <- lm(connectance ~ nbnode, network_metrics)
#summary(c_richness_mod)
#qplot(nbnode, connectance, data =  network_metrics, geom = "point") +
#  geom_smooth(method = 'lm') +
#  stat_poly_eq(
#    formula = formula,
#    eq.with.lhs = "italic(hat(y))~`=`~",
#    aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
#    parse = TRUE) +
#  xylabs(x = "nbnode", y = "connectance")
# Take residuals as Morris et al. (2014)
network_metrics$connectance_corrected <- residuals(c_richness_mod)

mysave(network_metrics, dir = dest_dir, overwrite = TRUE)

################################
#  Compute motif distribution  #
################################


cat("-----------------------\n")
cat("End of network metrics computation\n")
cat("-----------------------\n")
