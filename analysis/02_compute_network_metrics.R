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
#devtools::load_all()
source(mypath("R", "metaweb_build.R"))
source(mypath("R", "local_network_build.R"))


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
# Remove NA length:
n_old <- nrow(weight_analysis)
weight_analysis %<>% filter(!is.na(length))
cat((n_old - nrow(weight_analysis)) / n_old  * 100, " % of the individuals have been removed.\n")
cat("Since their length was NAs.\n")

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

if ("composition" %in% colnames(network_analysis)) {
 network_analysis %<>% select(-composition)
}
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
mysave(trophic_level, trophic_class, dir = data_common, overwrite = TRUE)

## Assign trophic group to each node  
trophic_level %<>%
  mutate(
    troph_group = get_size_class(trophic_level, NULL, troph_level, trophic_class)
  )

## Assign to each network its trophic group:
source('../analysis/misc/parallel_setup.R')

#if (!is.null(options("network.type")) & options("network.type") == "species") {
  #var_chr <- "species"
#} else {
  var_chr <- "sp_class"
#}
network_analysis %<>%
  mutate(
  composition = furrr::future_map(composition, function (compo, troph_group, var2join){
    if ("troph_group" %in% colnames(network_analysis)) {
      compo %<>% select(-troph_group)
    }
    left_join(compo, troph_group, by = var2join)
}, troph_group = trophic_level, var2join = var_chr))

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
    ungroup() 
  composition$troph_group <- get_size_class(composition, NULL, troph_level, trophic_class)

  composition %<>%
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
  filter(opcod == 2303)

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
    mean_troph_level = map_dbl(troph_level, mean),
    max_troph_lvl = map_dbl(troph_level, max),
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
m_richness_mod <- lm(modularity ~ log(nbnode) + I(log(nbnode)^2), network_metrics)
troph_lvl_richness_mod <- lm(troph_level_avg ~ log(nbnode) + I(log(nbnode)^2), network_metrics)
#troph_length_richness_mod <- lm(troph_length ~ log(nbnode) + I(log(nbnode)^2), network_metrics)
summary(troph_lvl_richness_mod)
formula <- y ~ x 
qplot(nbnode, troph_length, data =  network_metrics, geom = "point") +
  geom_smooth(method = 'lm', formula = formula) +
  stat_poly_eq(
    formula = formula,
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
    parse = TRUE) +
  xylabs(x = "nbnode", y = "connectance")
# Take residuals as Morris et al. (2014)
network_metrics$connectance_corrected <- residuals(c_richness_mod)
network_metrics$modularity_corrected <- residuals(m_richness_mod)
network_metrics$mean_troph_level_corrected <- residuals(troph_lvl_richness_mod)

mysave(network_metrics, dir = dest_dir, overwrite = TRUE)

#####################################
#  Compute species diet by network  #
#####################################

myload(network_analysis, dir = dest_dir)
# Get network
net <- network_analysis %>%
  unnest(network) %>%
  group_by(opcod)

# Match fish, plant and other animal as prey  
convert_sp_in_diet <- c(
  "[A-Z]{3}.{2}" = "fish",
  "phytopl" = "phyto_plankton",
  "phytob" = "phyto_benthos",
  "zoopl" = "zoo_plankton",
  "zoob" = "zoo_benthos"
)
if ("species" %in% colnames(net)) {
  names(convert_sp_in_diet[convert_sp_in_diet == "fish"]) <- "[A-Z]{3}"

}

net_diet <- net %>%
  mutate(
    diet = str_replace_all(from, convert_sp_in_diet),
    diet = ifelse(!diet %in% convert_sp_in_diet, NA, diet)
  )

# Exclude macrophages (macroph) from feeders since we do not have data about
# their biomass 
net_diet %<>%
  filter(!is.na(diet))
# Keep only the predators 
net_diet %<>%
  select(opcod, to, diet) %>%
  rename("predator" = "to")

# Add the biomass of the predators:
biomass_predator <- network_analysis %>%
  unnest(composition) %>%
  select(opcod, sp_class, biomass)

if ("species" %in% colnames(net)) {
  col_sp <- "species"
} else {
  col_sp <- "sp_class"
}
net_diet %<>%
  left_join(rename(biomass_predator, "predator" = col_sp), by = c("opcod", "predator"))
# Since we do not have biomass about compartment expect fishes, we will keep
#only the predators that match fish   
fish_id <- names(convert_sp_in_diet[convert_sp_in_diet == "fish"])
net_diet %<>%
  mutate(pred_fish = str_match(predator, fish_id)) %>%
  filter(!is.na(pred_fish))
# Ok, we have biomass for all the fishes of the network
stopifnot(filter(net_diet, is.na(biomass)) %>% nrow() == 0)

# Get the total biomass by opcod:
total_biomass <- biomass_predator %>%
  group_by(opcod) %>%
  summarise(total_biomass = sum(biomass))

# Compute the biomass by diet:
net_diet %<>%
  group_by(opcod, diet) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(opcod) %>%
  mutate(rel_biomass = biomass / sum(biomass))
# check that rel_biomass has been correctly computed:  
check_rel_biomass <- net_diet %>%
  group_by(opcod) %>%
  summarise(check = round(sum(rel_biomass), 2))
stopifnot(unique(check_rel_biomass$check) == 1)

myload(network_metrics, dir = dest_dir)

net_diet %<>%
  group_by(opcod) %>%
  nest(.key = "diet_composition")

network_metrics %<>%
  left_join(net_diet, by = "opcod")


####################################
#  Compute weighted trophic level  #
####################################

weighted_mean_trophic_lvl <- network_metrics %>%
  select(opcod, composition) %>%
  unnest(composition) %>%
  group_by(opcod) %>%
  summarise(w_trph_lvl_avg = sum(troph_level * biomass) / sum(biomass))
network_metrics %<>%
  left_join(weighted_mean_trophic_lvl, by = "opcod")

# Disantengle weighted avg trophic richness and number of species 
#formula <- y ~ x + I(x^2)
#qplot(log(nbnode), w_trph_lvl_avg, data =  network_metrics, geom = "point") +
  #geom_smooth(method = 'lm', formula = formula) +
  #stat_poly_eq(
    #formula = formula,
    #eq.with.lhs = "italic(hat(y))~`=`~",
    #aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
    #parse = TRUE) +
  #xylabs(x = "nbnode", y = "Avg trophic level")

#mod <- lm(w_trph_lvl_avg ~ I(nbnode^2) , network_metrics)
#par(mfrow = c(2,2))
#plot(mod)

mysave(network_metrics, dir = dest_dir, overwrite = TRUE)

################################
#  Compute motif distribution  #
################################


cat("-----------------------\n")
cat("End of network metrics computation\n")
cat("-----------------------\n")
