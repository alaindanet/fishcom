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

myload(op_analysis, metaweb_analysis, dir = data_common)
myload(network_metrics, dir = dest_dir)
op_analysis %<>%
  dplyr::select(opcod, station, year)
to_be_summarized <- c("nestedness", "connectance", "connectance_corrected", "nbnode",
  "mean_troph_level", "max_troph_level", "modularity")
com <- left_join(network_metrics, op_analysis, by = "opcod") %>%
  group_by(station) %>%
  rename(mean_troph_level = troph_level_avg,
    max_troph_level = troph_length) %>%
  summarise_at(to_be_summarized,
    funs(cv = sd(.) / mean(.), med = median))

temporal_network_metrics <- com
mysave(temporal_network_metrics, dir = dest_dir, overwrite = TRUE)

###############################################
#  Temporal network biomass by trophic group  #
###############################################
cat("---------------------------------------------\n")
cat("Temporal network biomass by trophic group\n")
cat("---------------------------------------------\n")

myload(op_analysis, metaweb_analysis, network_analysis, dir = data_common)
myload(network_metrics, dir = dest_dir)

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
  select_at(vars(dplyr::matches("biomass|richness_avg|station|troph_group")))

## Merge with temporal_network_metrics
biomass_variation %<>%
  group_by(station) %>%
  nest(.key = "troph_group")

myload(temporal_network_metrics, dir = dest_dir)
## Check if troph_group already exist:
if ("troph_group" %in% colnames(temporal_network_metrics)) {
  temporal_network_metrics %<>% dplyr::select(-matches("troph_group"))
}
temporal_network_metrics <-
  left_join(temporal_network_metrics, biomass_variation, by = "station")

mysave(temporal_network_metrics, dir = dest_dir, overwrite = TRUE)

###################################################
#  Compute temporal betadiversity of interaction  #
###################################################
cat("---------------------------------------------\n")
cat("Compute temporal betadiversity of interaction\n")
cat("---------------------------------------------\n")

# Compute betalink
myload(network_analysis, dir = dest_dir)
myload(op_analysis, dir = data_common)

net <- left_join(network_analysis, dplyr::select(op_analysis, opcod, station, year)) %>%
  ungroup()

source('../analysis/misc/parallel_setup.R')
## Get network as matrices
net %<>%
  mutate(
    network = map2(network, station, function(x, y) {
      message(sprintf('Station %s', y))
      igraph::graph_from_data_frame(x, directed = TRUE)
}
))
## betalink:
net %<>%
  select(network, station) %>%
  group_by(station) %>%
  nest() %>%
  mutate(betalink = map2(data, station,
      function (x, y) {
      message(sprintf("Station %s", y))
      betalink::network_betadiversity(x$network)
      }
      )
  ) %>%
  select(-data)

# Merge with temporal_network_metrics 
myload(temporal_network_metrics, dir = dest_dir)
temporal_network_metrics %<>%
  left_join(net, by = "station")

mysave(temporal_network_metrics, dir = dest_dir, overwrite = TRUE)

cat("-----------------------------------\n")
cat("End of temporal metrics computation\n")
cat("-----------------------------------\n")

################################################################
#  Compute temporal correlation between biomass trophic level  #
################################################################
cat("----------------------------------------------------------\n")
cat("Compute temporal correlation between biomass trophic level\n")
cat("----------------------------------------------------------\n")
myload(op_analysis, network_analysis, dir = data_common)
myload(network_metrics, dir = dest_dir)

op <- op_analysis %>% dplyr::select(opcod, station, date)
net <- left_join(network_metrics, op, by = "opcod") %>%
  ungroup() %>%
  filter(!is.na(station))

troph_group_biomass <- net %>%
  unnest(composition) 

# Set troph_group names in function of the number of group
unique_troph_group <- unique(troph_group_biomass$troph_group)
set_troph_names <- function (troph = NULL, nb_names = 2) {
  if (troph == 1) {
    return("low")
  } else if (troph == 2) {
    if (nb_names == 3) {
      return("medium")
    } else {
      return("high")
    }
  } else if (troph == 3) {
    return("high")
  }
}

troph_group_biomass %<>%
  group_by(station, troph_group, date) %>%
  summarise(biomass = mean(biomass)) %>%
  ungroup() %>%
  mutate(troph_group =
    map_chr(troph_group, ~set_troph_names(.x, nb_names = length(unique_troph_group)))
  )

complete_com <- troph_group_biomass %>%
  group_by(station) %>%
  summarise(
    troph_group = list(unique(troph_group)),
    date = list(unique(date))) %>%
  mutate(comb = map2(troph_group, date, function(sp, date){
    test <- expand.grid(troph_group = sp, date = date)
    return(test)
    })
    ) %>%
  unnest(comb)
# Join and put biomass to 0 when no observed:
complete_com %<>% left_join(troph_group_biomass, by = c("station", "troph_group", "date")) %>%
  mutate(biomass = ifelse(is.na(biomass), 0, biomass))

test <- complete_com %>%
  group_by(station, troph_group, date) %>%
  summarise(nobs = n())
complete_com %<>%
  group_by(station) %>%
  nest()

complete_com %<>%
  mutate(
    com_mat = purrr::map(data, function(x) spread(x, troph_group, biomass)),
    com_mat = purrr::map(com_mat, function(x) select(x, -date))
  )
complete_com$com_mat[[1]]
synchrony <- complete_com %>%
  mutate(
    avg_troph_group = purrr::map(com_mat, colMeans),
    cov_mat = purrr::map(com_mat, cov),
    var_troph_group = purrr::map(cov_mat, diag),
    synchrony = purrr::map_dbl(cov_mat, function(x) {
      com_var <- sum(x)
      var_intra_sp <- sum(sqrt(diag(x)))
      phi <- com_var / var_intra_sp^2 
      return(phi)
    }),
  cv_troph_group = purrr::map2_dbl(avg_troph_group, var_troph_group, function(biomass, variance) {
    #Check that the species are in the same order in the vector:
    stopifnot(names(biomass) == names(variance))

    rel_biomass <- biomass / sum(biomass)
    rel_sdt <- sqrt(variance) / biomass

    cv_avg <- sum(rel_biomass * rel_sdt)
    return(cv_avg)
    }),
  cv_com = synchrony * cv_troph_group
  )

synchrony %<>%
  select(station, synchrony, cv_troph_group, cv_com)
troph_group_synchrony <- synchrony

# ADD the diversity total and by troph_group
div_troph_group <- net %>%
  unnest(composition) %>%
  select(station, matches("species|sp_class"), troph_group, date) %>%
  mutate(troph_group = ifelse(troph_group == 2, "low", "high")) %>%
  group_by(station, troph_group, date) %>%
  summarise(nbsp = n()) %>%
  group_by(station, troph_group) %>%
  summarise(med_nbsp = median(nbsp)) %>%
  spread(troph_group, med_nbsp) %>%
  mutate(low = ifelse(is.na(low), 0, low)) %>%
  rename(med_nbsp_high_troph = high, med_nbsp_low_troph = low) %>%
  mutate(med_nbsp_total = med_nbsp_high_troph + med_nbsp_low_troph)

troph_group_synchrony %<>%
  left_join(div_troph_group, by = c("station"))

mysave(troph_group_synchrony, dir = mypath("data"), overwrite = TRUE)

cat("-----------------------------------\n")
cat("End of temporal metrics computation\n")
cat("-----------------------------------\n")

