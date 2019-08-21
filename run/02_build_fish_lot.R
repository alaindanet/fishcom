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
fish_length %<>% unnest(fish)
save(fish_length, file = mypath("data-raw", "fish_op_build", "fish_length.rda"))
cat("Done!")
