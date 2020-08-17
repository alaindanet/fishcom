################################################################################
#            Analyze community structure and its temporal variation            #
################################################################################

# Dep 
library(tidyverse)
library(magrittr)
library(lubridate)
library(furrr)
library(vegan)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "community_methods.R"))
source(mypath("R", "community_analysis.R"))
#devtools::load_all()

# Cores
cat("Working directory:\n")
cat(getwd(), "\n")
source(mypath("analysis", "misc","parallel_setup.R"))

#####################
#  Compute biomass  #
#####################

wl <- read_delim(mypath("data-raw","weight_length_coef.csv"),
  delim = ";",
  local = locale(decimal_mark = "."),
  col_types = "cddc"
)
wl %<>% dplyr::select(species_code, a, b) %>%
  rename(species = species_code)
myload(length_analysis, dir = data_common)

weight_analysis <- length_analysis %>%
  mutate(weight = calc_fish_body_mass(length)) %>%
  dplyr::select(opcod, species, length, weight)
# length is given in milimeters

mysave(weight_analysis, dir = data_common, overwrite = TRUE)

#########################################
#  Compute richness and beta-diversity  #
#########################################
# Caution: Here the richness of the community is different from the richness of
# the network beacause we do not have basal nodes.
#

# Get station and  
myload(length_analysis, op_analysis, dir = data_common)
op_analysis %<>%
  select(opcod, station, date, surface)

com_analysis <- length_analysis %>%
  group_by(opcod, species) %>%
  summarise(nind = n()) %>%
  left_join(., op_analysis, by = "opcod")

# Get biomass by species and by opcod and their average size
myload(weight_analysis, dir = data_common)
weight_analysis %<>%
  group_by(opcod, species) %>%
  summarise(biomass = sum(weight, na.rm =TRUE), length = mean(length, na.rm = TRUE))
# NA rm bc some fish have no valid measurement


## The biomass and length are given by species
com_analysis %<>%
  left_join(., weight_analysis, by = c("opcod", "species")) %>%
  select(opcod, species, nind, length, biomass)

#Rm species that had no valid measurements in an opcod
com_analysis %<>% filter( !is.nan(length))

# Save community_analysis
community_analysis <- com_analysis

mysave(community_analysis, dir = data_common, overwrite = TRUE)

###############################
#  Compute community metrics  #
###############################
# Summary information by fishing operation such as richness, biomass
myload(op_analysis, community_analysis, dir = mypath("data"))

com <- community_analysis %>%
  group_by(opcod) %>%
  summarise(
    richness = n(),
    nind = sum(nind),
    biomass = sum(biomass),
    #bm_std = sum(biomass)/surface
  ) %>%
  left_join(select(op_analysis, opcod, surface), by = "opcod") %>%
  mutate(
    rich_std = richness / surface, 
    nind_std = nind / surface,
    bm_std = biomass / surface
  ) 

######################
#  Compute evenness  #
######################

evenness <- community_analysis %>%
    select(opcod, species, biomass) %>%
    group_by(opcod, species) %>%
    summarise(biomass = sum(biomass)) %>%
    group_by(opcod) %>%
    nest() %>%
    mutate(
      sp_vector = furrr::future_map(data,
	function (.data) {
	  .data %<>% spread(species, biomass) 
	  stopifnot(nrow(.data) == 1)
	  return(unlist(.data))
	}
	),
      rel_bm = furrr::future_map(sp_vector,
	function(.data) {
	  return(.data / sum(.data))
	}
	),
      pielou = furrr::future_map(rel_bm, compute_pielou_simpson)
    )


evenness %<>%
  unnest(pielou) %>% 
  select(-data)

community_metrics <- left_join(com,
  evenness, by = "opcod")
mysave(community_metrics, dir = data_common, overwrite = TRUE)

############################
#  Compute beta-diversity  #
############################
myload(community_analysis, op_analysis, dir = mypath("data"))

# Compute beta-diversity
betadiv <- compute_temporal_betadiv(.op = op_analysis, com = community_analysis)

mysave(betadiv, dir = data_common, overwrite = TRUE)

############################################
#  Compute temporal community description  #
############################################
# Compute mean and cv of richness
myload(community_metrics, op_analysis, betadiv, dir = mypath("data"))

com <- summarise_com_over_time(
  op = op_analysis,
  com = community_metrics
)

com <- left_join(com, select(betadiv, -data, -com), by = "station")

temporal_community_metrics <- com
mysave(temporal_community_metrics, dir = data_common, overwrite = TRUE)

##################################
#  Compute population synchrony  #
##################################
# Following Thibault & Collonny (2013). Ecolet
source(mypath("R", "synchrony.R"))

myload(community_analysis, op_analysis, dir = mypath("data"))

synchrony <- compute_com_synchrony(.op = op_analysis, com = community_analysis)

mysave(synchrony, dir = mypath("data"), overwrite = TRUE)

##################
#  Compute nmds  #
##################
source(mypath("R", "community_analysis.R"))

com_mat <- compute_com_mat(.op = op_analysis, com = ungroup(community_analysis))
com <- select(com_mat, -station)
com <- round(com[, colSums(com != 0) > 0])

nmds <- vegan::metaMDS(com, k=3, trymax = 200)
mysave(nmds, com_mat, dir = mypath("report"), overwrite = TRUE)
