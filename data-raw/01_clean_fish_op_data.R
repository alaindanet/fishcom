################################################################################
#                       Clean fishing operation dataset                        #
################################################################################

# Adapted from the script of Willem Bonnafé:
# ../bonnafe_work/data/raw_data/cleanData.r

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "fishing_op_build")
source(mypath("R", "misc.R"))
source(mypath("R", "cleaning_methods.R"))

################################################################################
#                                 Fish length                                  #
################################################################################
myload(fish_length, dir = mypath("data-raw", "fishing_op_build"))

##############################
#  Remove migratory species  #
##############################
# Remove migratory species, species from lake, crayfish or very rare ones

fish_length %<>% dplyr::filter(!(species %in% sp_to_remove()))

################################
#  Regroup morphotype species  #
################################
#old = new

fish_length %<>%
  mutate(
    species = str_replace_all(species, sp_to_replace())
  )

##################
#  Obtain opcod  #
##################

myload(lot_id_opcod, op, dir = mypath("data-raw","fishing_op_build"))

fish_length %<>%
  left_join(lot_id_opcod) %>%
  rename(length = fish) %>%
  select(opcod, species, length)

# Keep only interesting operation:
fish_length %<>%
  filter(opcod %in% op$opcod)

########################################
#  Extract nb species and individuals  #
########################################

op_sp_ind <- fish_length %>%
  group_by(opcod, species) %>%
  summarise(nind = n()) %>%
  group_by(opcod) %>%
  summarise(
    nb_sp  = n(),
    nb_ind = sum(nind)
    )
mysave(op_sp_ind, dir = mypath("data"), overwrite = TRUE)

#######################
#  Nb ind by species  #
#######################

nb_ind_sp <- fish_length %>%
  group_by(species) %>%
  summarise(nind = n())


low_nb_ind <- filter(nb_ind_sp, nind <= 100)

fish_length %<>% filter(!species %in% low_nb_ind$species)

# Remove crazy length
filter(fish_length, length > 5000)
distri_sp_length <- fish_length %>%
  group_by(species) %>%
  summarise(
    avg = mean(length, na.rm = TRUE),
    sdt = sd(length, na.rm = TRUE))
# Remove it
fish_length %<>%
  left_join(distri_sp_length) %>%
  mutate(length = ifelse(length > avg + 5 * sdt | length < avg + 5 * sdt, NA, length)) %>%
  select(-avg, -sdt)

mysave(fish_length, nb_ind_sp, dir = mypath("data"), overwrite = TRUE)

################################################################################
#                                Operation data                                #
################################################################################

myload(lot_id_opcod, op, dir = mypath("data-raw","fishing_op_build"))

op %>% filter(is.na(date))
#Ok
mysave(op, dir = mypath("data-raw"), overwrite = TRUE)

################################################################################
#                            Operation description                             #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_desc.rda"))
op_desc
# Look ok

mysave(op_desc, dir = mypath("data-raw"), overwrite = TRUE)
################################################################################
#                         Operation environmental data                         #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_env.rda"))
op_env
colnames(op_env)
mysave(op_env, dir = mypath("data-raw"), overwrite = TRUE)

################################################################################
#                                   Station                                    #
################################################################################

load(mypath("data-raw", "fishing_op_build", "station.rda"))
library('sf')
mysave(station, dir = mypath("data-raw"), overwrite = TRUE)
