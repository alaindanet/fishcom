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


library(tidyverse)
library(magrittr)
library(furrr)

###############
#  Load data  #
###############
load(mypath("data-raw","fishing_op_build", "lot_measure.rda"))
load(mypath("data-raw","fishing_op_build", "fish_lot.rda"))

# For future:
source(mypath("R", "building_dataset.R"))
source(mypath("analysis", "misc", "parallel_setup.R"))

fish_lot %<>%
  mutate(
      fish = purrr::future_pmap(
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
	select(lop_id, lop_pre_id, species, fish) %>%
	unnest(fish)

save(mypath("data-raw","fishing_op_build", "fish_length.rda"))
cat("Done!")
