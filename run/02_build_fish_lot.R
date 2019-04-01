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
options(echo = TRUE)
default::default(unlist) <- list(use.names = FALSE)
mypath <- rprojroot::find_package_root_file
max_size_future_gb <- 2
options('future.globals.maxSize' = (max_size_future_gb*10^3)*1024^2)
options('future.globals.maxSize')

library(magrittr)

###############
#  Load data  #
###############
load(mypath("data-raw", "fishing_op_build", "lot_measure.rda"))
load(mypath("data-raw", "fishing_op_build", "fish_lot.rda"))

# For future:
source(mypath("R", "building_dataset.R"))
## Compile function:
gen_fish_from_lot <- compiler::cmpfun(gen_fish_from_lot)
source(mypath("analysis", "misc", "parallel_setup.R"))

fish_lot %<>%
  dplyr::mutate(
      fish = furrr::future_pmap(
    list(
      id = lop_id,
      type = type_lot,
      min_size = lop_longueur_specimens_taille_mini,
      max_size = lop_longueur_specimens_taille_maxi,
      nb = lop_effectif
      ),
    ~gen_fish_from_lot(id = ..1,
      type = ..2,
      min_size = ..3,
      max_size = ..4,
      nb = ..5,
      ind_measure = lot_measure,
      ind_size = mei_taille,
      ind_id = mei_lop_id,
      verbose = TRUE)
    )
      ) %>%
    dplyr::select(lop_id, lop_pre_id, species, fish) %>%
    tidyr::unnest(fish)

save(mypath("data-raw", "fishing_op_build", "fish_length.rda"))
cat("Done!")
