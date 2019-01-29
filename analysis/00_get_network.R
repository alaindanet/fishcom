################################################################################
#                      Get the metaweb and local network                       #
################################################################################

# Dep 
library(tidyverse)
library(magrittr)
devtools::load_all()


#############
#  Metaweb  #
#############
# Load data
data(pred_win)
data(diet_shift)
data(resource_diet_shift)
data(fish_length)

# Build metaweb
metaweb_analysis <- build_metaweb(
  data = fish_length,
  species = species,
  size = length,
  pred_win = pred_win,
  beta_min = beta_min,
  beta_max = beta_max,
  fish_diet_shift = fish_diet_shift,
  low_bound = size_min,
  upper_bound = size_max,
  fish = fish,
  resource_diet_shift = resource_diet_shift,
  class_method = "percentile",
  nb_class = 9,
  pred_win_method = "midpoint",
  fish_resource_method = "midpoint",
  na.rm = TRUE,
  replace_min_by_one = FALSE)

devtools::use_data(metaweb_analysis, overwrite = TRUE)
rm(list = ls())

###################
#  Local network  #
###################

# Build local network
data(length_analysis)
data(op_analysis)
## Get station id for the op
op_analysis %<>% select(opcod, station)
length_analysis %<>%
  left_join(., op_analysis, by = "opcod")
rm(op_analysis)
##
data(metaweb_analysis)
filter(classes, is.na(class_id))
qplot(x = length, data = filter(classes, species == "TRF"), geom = "histogram")
filter(classes, species == "TRF") %>% map(., summary)
filter(metaweb_analysis$size_class, species == "TRF")
network_analysis <- build_local_network(
  data = length_analysis,
  species = species,
  var = length,
  group_var = opcod,
  metaweb = metaweb_analysis,
  classes = NULL,
  out_format = "igraph"
)
devtools::use_data(network_analysis, overwrite = TRUE)
