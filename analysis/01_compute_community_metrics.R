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

# Sanitize check to be coherent with network:
weight_analysis %<>%
  filter(!species %in% "OBL") %>%
  filter(!length < 1)

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
  summarise(biomass = sum(weight), length = mean(length))
## The biomass and length are given by species
com_analysis %<>%
  left_join(., weight_analysis, by = c("opcod", "species")) %>%
  select(opcod, species, nind, length, biomass)

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

community_metrics <- com
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
  nest()
filter(com, station == 8442) %>%
  unnest() %>%
  group_by(species, date) %>%
  summarise(n = n()) %>%
  filter(n != 1)
## build community matrices
com %<>%
  mutate(
    com = furrr::future_map2(data, station, function(x, y) {
      message(sprintf('Station %s', y))

      x %<>% spread(species, nind) %>%
	mutate_if(is.integer, list(~replace(.,is.na(.), as.integer(0)))) %>%
	arrange(date) %>%
	select(-date)
      x
}
    )
  )
#
compute_betadiv <- function(com, binary = FALSE, time_to_time = FALSE) {
  if (binary) {
    com %<>% replace(., . != 0, 1)
  }

  dist_mat <- vegan::vegdist(com, method = "bray", binary = binary)

  if (time_to_time) {
    dist_mat <- diag(dist_mat)
  }

  mean(dist_mat)
}
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
  group_by(station) %>%
  select(station, richness, nind, biomass) %>%
  summarise_at(c("richness", "nind", "biomass"),
    funs(avg = mean, cv = sd(.) / mean(.), med = median, stab = mean(.) / sd(.)))

com <- left_join(com, select(betadiv, -data, -com), by = "station")

# Strange biomass cv: 
filter(com, biomass_cv > 1.2)
left_join(community_metrics, op_analysis, by = c("opcod")) %>%
  filter(station == 1706)
filter(community_analysis, opcod == 4223)
# There was 57 ANG, and pretty big ones!

temporal_community_metrics <- com
mysave(temporal_community_metrics, dir = data_common, overwrite = TRUE)

##################################
#  Compute population synchrony  #
##################################
# Following Thibault & Collonny (2013). Ecolet
source(mypath("R", "synchrony.R"))

myload(community_analysis, op_analysis, dir = mypath("data"))

com <- ungroup(community_analysis) %>%
  left_join(select(op_analysis, opcod, station, date), by = "opcod") %>%
  filter(!is.na(station)) %>%
  select(station, date, species, biomass)

# Get 0 biomass when the species is absent of station
# By station replicate complete species list observed:
complete_com <- com %>%
  group_by(station) %>%
  summarise(
    species = list(unique(species)),
    date = list(unique(date))) %>%
  mutate(comb = map2(species, date, function(sp, date){
    test <- expand.grid(species = sp, date = date)
    return(test)
    })
    ) %>%
  unnest(comb)
# Join and put biomass to 0 when no observed:
complete_com %<>% left_join(com, by = c("station", "species", "date")) %>%
  mutate(biomass = ifelse(is.na(biomass), 0, biomass))

test <- complete_com %>%
  group_by(station, species, date) %>%
  summarise(nobs = n())
filter(test, nobs > 1)
complete_com %<>%
  group_by(station) %>%
  nest()

complete_com %<>%
  mutate(
    com_mat = purrr::map(data, function(x) spread(x, species, biomass)),
    com_mat = purrr::map(com_mat, function(x) select(x, -date)),
    com_mat = purrr::map(com_mat, as.matrix)
  )
synchrony <- complete_com %>%
  mutate(
    avg_sp = purrr::map(com_mat, colMeans),
    cov_mat = purrr::map(com_mat, cov),
    var_sp = purrr::map(cov_mat, diag),
    synchrony = purrr::map_dbl(cov_mat, compute_synchrony),
    cv_sp = purrr::map2_dbl(avg_sp, var_sp, compute_avg_cv_sp),
    cv_com = compute_cv_com(synchrony = synchrony, cv_sp = cv_sp),
    cv_classic = purrr::map2_dbl(cov_mat, com_mat, function(variance, biomass) {
      sqrt(sum(variance)) / mean(rowSums(biomass))
    })
  )

synchrony %<>%
  select(station, synchrony, cv_sp, cv_com, cv_classic)
mysave(synchrony, dir = mypath("data"), overwrite = TRUE)
