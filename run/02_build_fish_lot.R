#!/usr/bin/env Rscript
################################################################################
#                        Build fish size since fish lot                        #
################################################################################

# Args should be to specify the type of cluster you are on or at home in order
# to find the cpus
# See: ?availableCores
args <- commandArgs(trailingOnly=TRUE)
if (!is.null(args) & length(args) > 1) {
  stop("Only one argument should be specified")
} else if (!is.null(args) & is.character(args)) {
  options(future.availableCores.methods = args[1])
}
options(echo = TRUE, warn = 1)
default::default(unlist) <- list(use.names = FALSE)
mypath <- rprojroot::find_package_root_file
max_size_future_gb <- 2
options('future.globals.maxSize' = (max_size_future_gb*10^3)*1024^2)
options('future.globals.maxSize')
options(nwarnings = 100000)

library(magrittr)
library(future)
library(tictoc)
library(tidyverse)
source(mypath("R", "misc.R"))

###############
#  Load data  #
###############
load(mypath("data-raw", "fishing_op_build", "lot_measure.rda"))
load(mypath("data-raw", "fishing_op_build", "fish_lot.rda"))

# For future:
source(mypath("R", "building_dataset.R"))
## Compile function:
gen_fish_from_lot <- compiler::cmpfun(gen_fish_from_lot)
#source(mypath("analysis", "misc", "parallel_setup.R"))

library(parallel)
options(mc.cores = 29)

tic()
fish_length <-
  get_size_from_lot(
    lot = fish_lot,
    id_var = lop_id,
    type_var = type_lot,
    nb_var = lop_effectif,
    min_var = lop_longueur_specimens_taille_mini,
    max_var = lop_longueur_specimens_taille_maxi,
    species = species,
    measure = lot_measure,
    measure_id_var = mei_lop_id,
    size_var = mei_taille,
    future_enabled = FALSE 
  )
toc()
#sequential: 37k sec and 47k sec if the gen_fish_from_lot is not compiled.
#fish_length$length_temp <- sapply(fish_length$fish, function (x) {
  #if (is.numeric(x)) { x } else { NA }
  #})
#fish_length %<>% unnest(length_temp)
save(fish_length, file = mypath("data", "fish_length.rda"))
cat("Done!")

#################
##  Miss lot N  #
#################
#test <- fish_length %>%
  #filter(is.na(length_temp))
#test2 <- fish_lot %>%
  #filter(lop_id %in% test$lop_id, type_lot == "N") %>%
  #group_by(type_lot)
#lot_measure %>%
  #filter(mei_lop_id %in% test2$lop_id)
#fish_test <-
  #get_size_from_lot(
    #lot = filter(fish_lot, lop_id %in% test$lop_id, type_lot == "N"),
    #id_var = lop_id,
    #type_var = type_lot,
    #nb_var = lop_effectif,
    #min_var = lop_longueur_specimens_taille_mini,
    #max_var = lop_longueur_specimens_taille_maxi,
    #species = species,
    #measure = filter(lot_measure, mei_lop_id %in% test2$lop_id),
    #measure_id_var = mei_lop_id,
    #size_var = mei_taille,
    #future_enabled = FALSE 
  #)
#missing_N_lot <- fish_test %>%
  #unnest(fish)

#fish_temp <- bind_rows(
  #filter(fish_length, !lop_id %in% missing_N_lot$lop_id),
  #rename(missing_N_lot, length_temp = fish)
#)

############
##  Lot G  #
############

#test <- fish_length %>%
  #filter(is.na(length_temp))
#statistic <- fish_lot %>%
  #filter(lop_id %in% test$lop_id) %>%
  #group_by(type_lot) %>%
  #summarise(n = n())

#to_recompute <- fish_lot %>%
  #filter(lop_id %in% test$lop_id, type_lot == "G") %>%
  #select(lop_id, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi, lop_effectif) %>%
  #filter(lop_effectif >= 5)

#fish_test <-
  #get_size_from_lot(
    #lot = filter(fish_lot, lop_id %in% to_recompute$lop_id),
    #id_var = lop_id,
    #type_var = type_lot,
    #nb_var = lop_effectif,
    #min_var = lop_longueur_specimens_taille_mini,
    #max_var = lop_longueur_specimens_taille_maxi,
    #species = species,
    #measure = lot_measure,
    #measure_id_var = mei_lop_id,
    #size_var = mei_taille,
    #future_enabled = FALSE 
  #)
#missing_G_lot <- fish_test %>%
  #unnest(fish)

#fish_temp <- bind_rows(
  #filter(fish_temp, !lop_id %in% missing_G_lot$lop_id),
  #rename(missing_G_lot, length_temp = fish)
#)

##################
##  Missing lot I  #
##################
#test <- fish_length %>%
  #filter(is.na(length_temp))
#test2 <- fish_lot %>%
  #filter(lop_id %in% test$lop_id) %>%
  #group_by(type_lot) %>%
  #summarise(n = n())
#lot_measure %>%
  #filter(mei_lop_id %in% test2$lop_id)

#to_recompute <- fish_lot %>%
  #filter(lop_id %in% test$lop_id, type_lot == "I") %>%
  #select(lop_id, lop_effectif)


#fish_test <-
  #get_size_from_lot(
    #lot = filter(fish_lot, lop_id %in% to_recompute$lop_id),
    #id_var = lop_id,
    #type_var = type_lot,
    #nb_var = lop_effectif,
    #min_var = lop_longueur_specimens_taille_mini,
    #max_var = lop_longueur_specimens_taille_maxi,
    #species = species,
    #measure = lot_measure,
    #measure_id_var = mei_lop_id,
    #size_var = mei_taille,
    #future_enabled = FALSE 
  #)
#missing_I_lot <- fish_test %>%
  #unnest(fish)

#fish_temp <- bind_rows(
  #filter(fish_temp, !lop_id %in% missing_I_lot$lop_id),
  #rename(missing_I_lot, length_temp = fish)
#)

#################
##  Missing S/L #
#################

## HERE
#to_recompute <- fish_lot %>%
  #filter(lop_id %in% test$lop_id, type_lot == "S/L") %>%
  #select(lop_id, lop_effectif)
#options(mc.cores = 1)

#fish_test <-
  #get_size_from_lot(
    #lot = filter(fish_lot, lop_id %in% to_recompute$lop_id),
    #id_var = lop_id,
    #type_var = type_lot,
    #nb_var = lop_effectif,
    #min_var = lop_longueur_specimens_taille_mini,
    #max_var = lop_longueur_specimens_taille_maxi,
    #species = species,
    #measure = lot_measure,
    #measure_id_var = mei_lop_id,
    #size_var = mei_taille,
    #future_enabled = FALSE 
  #)
#missing_SL_lot <- fish_test %>%
  #unnest(fish)

