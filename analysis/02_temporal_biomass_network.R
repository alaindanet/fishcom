################################################################################
#             Analyze the biomass and network temporal variation               #
################################################################################

# Dep 
library(tidyverse)
library(ggpmisc)
library(cowplot)
library(magrittr)
library(igraph)
library(lubridate)
devtools::load_all()
theme_set(theme_alain())

###############################
#  Match biomass and network  #
###############################

# Load
data(weight_analysis)
data(metaweb_analysis)
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

biomass <- left_join(weight_analysis, st_timing, by = "opcod") %>%
  group_by(opcod, station, times) %>%
  nest()
biomass %<>%
  mutate(
    biomass = map_dbl(data, function(x) sum(x$weight))
  ) %>%
  dplyr::select(-data)
biomass_analysis <- biomass
devtools::use_data(biomass_analysis, overwrite = TRUE)
  
