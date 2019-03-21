################################################################################
#                Compute the temporal properties of the network                #
################################################################################

library(tidyverse)
library(magrittr)
library(igraph)
library(furrr)
library(betalink)
library(tictoc)
devtools::load_all()

# Cores

######################################
#  Temporal network characteristics  #
######################################

data(network_metrics)
data(op_analysis)
op_analysis %<>%
  dplyr::select(opcod, station, year)
to_be_summarized <- c("nestedness", "connectance", "connectance_corrected", "nbnode",
  "mean_troph_level", "max_troph_level", "modularity")
com <-
    left_join(network_metrics, op_analysis, by = "opcod") %>%
  group_by(station) %>%
  rename(mean_troph_level = troph_level_avg,
    max_troph_level = troph_length) %>%
  summarise_at(to_be_summarized,
    funs(cv = sd(.) / mean(.), med = median))

temporal_network_metrics <- com
devtools::use_data(temporal_network_metrics, overwrite = TRUE)
rm(list = ls())

###############################################
#  Temporal network biomass by trophic group  #
###############################################
data(network_analysis)
data(op_analysis)

op <- op_analysis %>% dplyr::select(opcod, station, year)
net <- left_join(network_analysis, op, by = "opcod") %>%
  ungroup()
rm(op_analysis, network_analysis)

net %<>% dplyr::select(station, troph_group) %>%
  unnest()
biomass_variation <- net %>%
  group_by(station, troph_group) %>%
  summarise_all(funs(avg = mean, cv = sd(.) / mean(.), stab = mean(.) / sd(.)))
## Be careful, at which point we compute biomass cv by trophic group. Each group
## should have at least 5 observations to get a reliable variance. This is
## especially true for trophic group 1 (lowest) which is often absent from the
## dataset
check_obs <- net %>%
  group_by(station, troph_group) %>%
  summarise(nobs = n()) %>%
  mutate(enough_obs = ifelse(nobs >= 5, TRUE, FALSE))

filter(biomass_variation, is.na(troph_group))
## Let's put values to NA when there is not enough observations:
biomass_variation %<>%
  left_join(., check_obs, by = c("station", "troph_group")) %>%
  mutate_at(vars(biomass_avg:nbnode_stab), funs(if_else(enough_obs, ., NA_real_))) %>%
  select_at(vars(matches("biomass|richness_avg|station|troph_group")))

## Merge with temporal_network_metrics
biomass_variation %<>%
  group_by(station) %>%
  nest(.key = "troph_group")

data(temporal_network_metrics)
## Check if troph_group already exist:
if ("troph_group" %in% colnames(temporal_network_metrics)) {
  temporal_network_metrics %<>% dplyr::select(-matches("troph_group"))
}
temporal_network_metrics <-
  left_join(temporal_network_metrics, biomass_variation, by = "station")

devtools::use_data(temporal_network_metrics, overwrite = TRUE)
rm(list = ls())

###################################################
#  Compute temporal betadiversity of interaction  #
###################################################

# Compute betalink
data(network_analysis)
data(op_analysis)

net <- left_join(network_analysis, dplyr::select(op_analysis, opcod, station, year)) %>%
  ungroup()

source('../analysis/misc/parallel_setup.R')
## Get network as matrices
net %<>%
  mutate(
    network = furrr::future_map2(network, station, function(x, y) {
      message(sprintf('Station %s', y))
      igraph::graph_from_data_frame(x, directed = TRUE)
}
))
## betalink:
net %<>%
  group_by(station) %>%
  nest() %>%
  mutate(betal = furrr::future_map2(data, station,
      function (data, station) {
      message(sprintf("Station %s", station))
      betalink::network_betadiversity(data$network)
      }
      )
  )
# Merge with network_metrics 
