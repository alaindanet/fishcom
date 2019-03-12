################################################################################
#            Analyze community structure and its temporal variation            #
################################################################################

# Dep 
library(tidyverse)
library(magrittr)
library(lubridate)
library(furrr)
library(vegan)
devtools::load_all()

# Cores
source('../analysis/misc/parallel_setup.R')

#####################
#  Compute biomass  #
#####################

wl <- read_delim("../data-raw/weight_length_coef.csv",
  delim = ";",
  local = locale(decimal_mark = "."),
  col_types = "cddc"
)
wl %<>% dplyr::select(species_code, a, b) %>%
  rename(species = species_code)
data(length_analysis)

weight_analysis <- length_analysis %>%
  left_join(., wl, by = "species") %>%
  mutate(
    weight = a * (length ^ b), #in miligrams
    weight = weight * 10 ^ -3 #in grams
  ) %>%
  dplyr::select(opcod, species, length, weight)
# length is given in milimeters

devtools::use_data(weight_analysis, overwrite = TRUE)

#########################################
#  Compute richness and beta-diversity  #
#########################################
# Caution: Here the richness of the community is different from the richness of
# the network beacause we do not have basal nodes.
#

# Get station and  
data(length_analysis)
data(op_analysis)
op_analysis %<>%
  select(opcod, station, year)

com_analysis <- length_analysis %>%
  group_by(opcod, species) %>%
  summarise(nind = n()) %>%
  left_join(., op_analysis, by = "opcod")

# Get biomass by species and by opcod and their average size
data(weight_analysis)
weight_analysis %<>% group_by(opcod, species) %>%
  summarise(biomass = sum(weight), length = mean(length))
## The biomass and length are given by species
com_analysis %<>%
  left_join(., weight_analysis, by = c("opcod", "species")) %>%
  select(opcod, species, nind, length, biomass)

# Save community_analysis
community_analysis <- com_analysis
devtools::use_data(community_analysis, overwrite = TRUE)

###############################
#  Compute community metrics  #
###############################
# Summary information by fishing operation such as richness, biomass
data(community_analysis)

com <- community_analysis %>%
  group_by(opcod) %>%
  summarise(
    richness = n(),
    nind = sum(nind),
    biomass = sum(biomass)
  )

community_metrics <- com
devtools::use_data(community_metrics, overwrite = TRUE)

############################
#  Compute beta-diversity  #
############################
data(community_analysis)
data(op_analysis)

#join op and community and get date!
## Get date
### There are no multiple sampling by year:
test <- op_analysis %>%
  group_by(opcod, station, year) %>%
  summarise(nb = n()) %>%
  ungroup %>%
  select(nb) %>%
  unlist
which(test > 1)

op_analysis %<>%
  select(opcod, station, year)

# Compute beta-diversity 
com <- left_join(community_analysis, op_analysis, by = "opcod") %>%
  select(-length) %>%
  group_by(station) %>%
  nest()

## build community matrices
com %<>%
  mutate(
    com = furrr::future_map2(data, station, function(x, y) {
      message(sprintf('Station %s', y))
      com_matrix <- x %>%
    rowid_to_column() %>%
    spread(species, nind) %>%
    select(-rowid) %>%
    group_by(year) %>%
    summarise_all(funs(sum(., na.rm =TRUE))) %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
  com_matrix
}
    )
  )
# Here I take the overall mean of differences between years but could be an idea
# to consider the mean of dissimilarity year to year (i.e. the diagonal values)
betadiv <- com %>%
  mutate(
    betadiv = furrr::future_map_dbl(com, function(com){
      vegan::vegdist(com, method = "bray", binary = FALSE) %>% mean
    })
  )


############################################
#  Compute temporal community description  #
############################################
# Compute mean and cv of richness
data(community_metrics)
data(op_analysis)

com <- left_join(community_metrics, op_analysis, by = c("opcod")) %>%
  group_by(station) %>%
  select(station, richness, nind, biomass) %>%
  summarise_at(c("richness", "nind", "biomass"),
    funs(avg = mean, cv = sd(.) / mean(.), med = median, stab = mean(.) / sd(.)))

com <- left_join(com, select(betadiv, station, betadiv), by = "station")

# Strange biomass cv: 
filter(com, biomass_cv > 1.2)
left_join(community_metrics, op_analysis, by = c("opcod")) %>%
  filter(station == 1706)
filter(community_analysis, opcod == 4223)
# There was 57 ANG, and pretty big ones!

temporal_community_metrics <- com
devtools::use_data(temporal_community_metrics, overwrite = TRUE)
