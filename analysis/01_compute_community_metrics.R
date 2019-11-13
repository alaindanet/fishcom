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
  #left_join(., wl, by = "species") %>%
  mutate(
    #weight = a * (length ^ b), #in miligrams
    weight = 0.01 * (length ^ 3.03), #in miligrams
    weight = weight * 10 ^ -3 #in grams
  ) %>%
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
  select(opcod, station, date)

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
myload(community_analysis, dir = mypath("data"))

com <- community_analysis %>%
  group_by(opcod) %>%
  summarise(
    richness = n(),
    nind = sum(nind),
    biomass = sum(biomass)
  )

######################
#  Compute evenness  #
######################

evenness <- community_analysis %>%
    select(opcod, species, nind) %>%
    group_by(opcod, species) %>%
    summarise(nind = sum(nind)) %>%
    group_by(opcod) %>%
    nest() %>%
    mutate(
    pielou = furrr::future_map(data, function(.data) {
      .data %<>% spread(species, nind) %>%
	mutate_if(is.integer, list(~replace(.,is.na(.), as.integer(0))))

      richness <- vegan::specnumber(.data)

      if (richness == 1) {
	pielou <- 0
	simpson <- 0
      } else {
	pielou <- vegan::diversity(.data) / log(richness)
	simpson <- vegan::diversity(.data, index = "simpson")
      }

      out <- tibble( pielou = pielou, simpson = simpson)
      return(out)
    })
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
com <- left_join(ungroup(community_analysis),
  select(op_analysis, opcod, station, date), by = "opcod") %>%
  filter(!is.na(station)) %>%
  select(station, date, species, nind) %>%
  group_by(station, date, species) %>%
  summarise(nind = sum(nind)) %>%
  group_by(station) %>%
  arrange(date) %>%
  nest()

## build community matrices
com %<>%
  mutate(
    com = furrr::future_map2(data, station, function(x, y) {

      x %<>% spread(species, nind) %>%
	mutate_if(is.integer, list(~replace(.,is.na(.), as.integer(0)))) %>%
	arrange(date) %>%
	select(-date)
      x
}
    )
  )
#
# Here I take the overall mean of differences between years but could be an idea
# to consider the mean of dissimilarity year to year (i.e. the diagonal values)
betadiv <- com %>%
  mutate(
    betadiv = furrr::future_map_dbl(com, compute_betadiv),
    betadiv_bin = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE),
    betadiv_diag = furrr::future_map_dbl(com, compute_betadiv, time_to_time = TRUE),
    betadiv_bin_diag = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE, time_to_time = TRUE)
  )


############################################
#  Compute temporal community description  #
############################################
# Compute mean and cv of richness
myload(community_metrics, op_analysis, dir = mypath("data"))

com <- left_join(community_metrics, op_analysis, by = c("opcod")) %>%
  select(station, richness, nind, biomass, pielou, simpson) %>%
  group_by(station) %>%
  summarise_at(c("richness", "nind", "biomass", "pielou", "simpson"),
    list(avg = mean, cv =~ sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))

com <- left_join(com, select(betadiv, -data, -com), by = "station")

temporal_community_metrics <- com
mysave(temporal_community_metrics, dir = data_common, overwrite = TRUE)

##################################
#  Compute population synchrony  #
##################################
# Following Thibault & Collonny (2013). Ecolet
source(mypath("R", "synchrony.R"))

myload(community_analysis, op_analysis, dir = mypath("data"))

synchrony <- get_sync_cv_mat(com_analysis = community_analysis,
  op_analysis = op_analysis,
  presence_threshold = 0.5)
synchrony[1,]$com_mat

synchrony %<>%
  select(station, synchrony, cv_sp, cv_com, cv_classic)

mysave(synchrony, dir = mypath("data"), overwrite = TRUE)
