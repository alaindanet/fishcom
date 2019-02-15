################################################################################
#             Analyze the biomass and network temporal variation               #
################################################################################

# Dep 
library(tidyverse)
library(magrittr)
library(lubridate)
library(furrr)
options(mc.cores = 3)
devtools::load_all()
update.packages
###############################
#  Match biomass and network  #
###############################

# Load
data(weight_analysis)
data(metaweb_analysis)
# compute weight by node and by opcod
weight_analysis %<>% assign_size_class(., species, var = length,
  classes = metaweb_analysis$size_class) %>%
  unite(sp_class, species, class_id, sep = "_") %>%
  dplyr::select(opcod, sp_class, weight)

# Get biomass by op
data(op_analysis)
st_timing <- op_analysis %>%
  dplyr::select(opcod, station, year, month) %>%
  unite(year_month, year, month, sep = "-") %>%
  mutate(times = ymd(paste0(year_month, "-01"))) %>%
  dplyr::select(-year_month)
rm(op_analysis)

# Sum biomass by op
biomass <- left_join(weight_analysis, st_timing, by = "opcod") %>%
  group_by(opcod, station, times) %>%
  nest()
biomass_analysis <- biomass %>%
  mutate(
    biomass = map_dbl(data, function(x) sum(x$weight))
  ) %>%
  dplyr::select(-data)
devtools::use_data(biomass_analysis, overwrite = TRUE)
  
# Get biomass by node associated by network
plan(multiprocess) # Do it in parallel
biomass_node <- biomass %>%
  rename(biomass = data) %>%
  unnest() %>%
  select(opcod, sp_class, weight) %>%
  group_by(opcod, sp_class) %>%
  nest() 
biomass_node %<>%
  mutate(biomass = future_map_dbl(data, colSums)) %>%
  select(-data) %>%
  ungroup()
future:::ClusterRegistry("stop")

# Associate biomass to network 
data(network_analysis)
biomass_node %<>%
  group_by(opcod) %>%
  nest(.key = biomass)
network_analysis <- left_join(network_analysis, biomass_node, by = "opcod")

# Get node abondance
## do it like this because of dplyr bug: 
## https://github.com/tidyverse/dplyr/issues/2080  
network_analysis$abundance <- map(network_analysis$data, function (x)
    x %>%
      unite(sp_class, species, class_id, sep = "_") %>%
      group_by(sp_class) %>%
      summarise(nind = n())
  ) %>% select(- data)
# merge biomass and abundance in a community variable
network_analysis %<>%
  mutate(community = map2(biomass, abundance, function (bio, abun){
      stopifnot(nrow(bio) == nrow(abun))
      left_join(bio, abun, by = "sp_class")
})) %>%
  select(-biomass, -abundance)

devtools::use_data(network_analysis, overwrite = TRUE)
