################################################################################
#                           Compute network metrics                            #
################################################################################


# Dep 
library(tidyverse)
library(magrittr)
library(igraph)
library(rnetcarto)
library(furrr)
library(tictoc)
devtools::load_all()

# Cores
## Check if at home:
if (!all(is.na(str_match(getwd(), "Documents")))) {
  options(mc.cores = parallel::detectCores() - 1)
}


#########################################
#  Add node biomass and node abundance  #
#########################################


# Load
data(weight_analysis)
data(metaweb_analysis)
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
data(op_analysis)
st_timing <- op_analysis %>%
  dplyr::select(opcod, station, year, month) %>%
  unite(year_month, year, month, sep = "-") %>%
  mutate(times = lubridate::ymd(paste0(year_month, "-01"))) %>%
  dplyr::select(-year_month)
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


devtools::use_data(network_analysis, overwrite = TRUE)

######################################
#  Network biomass by trophic group  #
######################################

data(network_analysis)
data(metaweb_analysis)

# Get biomass by trophic level
## Compute trophic level by node
g <- igraph::graph_from_adjacency_matrix(metaweb_analysis$metaweb,
  mode = "directed")
dead_material <- c("det", "biof")
## Compute trophic level by node with metaweb
trophic_level <- NetIndices::TrophInd(metaweb_analysis$metaweb, Dead = dead_material) %>%
  mutate(sp_class = colnames(metaweb_analysis$metaweb)) %>%
  rename(troph_level = TL) %>%
  select(sp_class, troph_level)
summary(trophic_level)
## Split trophic level in three classes:
trophic_class <- split_in_classes(trophic_level$troph_level, class_method = "percentile",
  nb_class = 3, round_limits = FALSE)
devtools::use_data(trophic_class, overwrite = TRUE)

## Assign trophic group to each node  
trophic_level %<>%
  mutate(
    troph_group = get_size_class(trophic_level, NULL, troph_level, trophic_class)
  )

## Assign to each network its trophic group:
network_analysis %<>%
  mutate(
  composition = furrr::future_map(composition, function (compo, troph_group){
    left_join(compo, troph_group, by = "sp_class")
}, troph_group = trophic_level))
## Sum biomass by trophic group:
### I do this way bc of a dplyr bug with n() in nested data.frame
network_analysis$troph_group <- map(network_analysis$composition, function(compo) {
      compo %<>%
	group_by(troph_group) %>%
	summarise(
	  biomass = sum(biomass),
	  nbnode  = n(),
	  richness = str_extract_all(sp_class, "[A-Z]") %>% unique %>% length
	  ) %>%
	ungroup()
})

network_analysis %>%
  unnest(troph_group) %>%
  filter(is.na(troph_group))
network_analysis %>%
  unnest(composition) %>%
  filter(opcod == 25770)

network_analysis %<>% dplyr::select(-composition)
devtools::use_data(network_analysis, overwrite = TRUE)

#####################
#  Network metrics  #
#####################

library(NetIndices)

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
    compartiment = map_dbl(metrics, "Cbar"),
    troph_level = future_map(network, NetIndices::TrophInd),
    troph_level = map(troph_level, "TL"),
    troph_level_avg = map_dbl(troph_level, mean),
    troph_length = map_dbl(troph_level, max),
    modularity = map_dbl(modul_guimera, 2)
  )

network_metrics <- network_analysis %>%
  dplyr::select(-network, -metrics, -troph_level)
devtools::use_data(network_metrics, overwrite = TRUE)
rm(list = ls())

##############################
#  Standardized connectance  #
##############################
data(network_metrics)

c_richness_mod <- lm(connectance ~ nbnode, network_metrics)
summary(c_richness_mod)
qplot(nbnode, connectance, data =  network_metrics, geom = "point") +
  geom_smooth(method = 'lm') +
  stat_poly_eq(
    formula = formula,
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
    parse = TRUE) +
  xylabs(x = "nbnode", y = "connectance")
# Take residuals as Morris et al. (2014)
network_metrics$connectance_corrected <- residuals(c_richness_mod)

devtools::use_data(network_metrics, overwrite = TRUE)
rm(list = ls())

################################
#  Compute motif distribution  #
################################


