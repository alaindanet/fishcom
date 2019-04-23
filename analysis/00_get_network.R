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
cat("Load data\n")
myload(pred_win, diet_shift, resource_diet_shift, length_analysis, dir = data_common)
cat("Load data: done\n")

# Build metaweb
metaweb_analysis <- build_metaweb(
  data = length_analysis,
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

mysave(metaweb_analysis, dir = data_common, overwrite = TRUE)

cat("Metaweb: done\n")
###################
#  Local network  #
###################
cat("Build local network\n")

# Build local network

myload(length_analysis, op_analysis, metaweb_analysis, dir = data_common)
## Get station id for the op
op_analysis %<>% select(opcod, station)
length_analysis %<>%
  left_join(., op_analysis, by = "opcod")
rm(op_analysis)
##
## Check if NA in the metaweb:
if (any(is.na(metaweb_analysis$size_class))) {
  stop("NA in class id, something went wrong")
}

qplot(x = length, data = filter(length_analysis, species == "TRF"), geom = "histogram")
filter(length_analysis, species == "TRF") %>% map(., summary)
filter(length_analysis, species == "TRF", is.na(length))
filter(metaweb_analysis$size_class, species == "TRF")
filter(metaweb_analysis$size_class, species == "IDE")
filter(length_analysis, species == "IDE")

network_analysis <- build_local_network(
  data = length_analysis,
  species = species,
  var = length,
  group_var = opcod,
  metaweb = metaweb_analysis,
  classes = NULL,
  out_format = "igraph"
)
network_analysis %>% unnest(data) %>%
  filter(is.na(class_id))


mysave(network_analysis, dir = dest_dir, overwrite = TRUE)
cat("Local network: done\n")
