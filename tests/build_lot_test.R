
library('tidyverse')
library('magrittr')
mypath <- rprojroot::find_package_root_file
dest_dir <- mypath("data-raw", "fishing_op_build")

load(mypath("data-raw", "fishing_op_build", "lot_testing.rda"))
load(mypath("data-raw", "fishing_op_build", "ind_measure_testing.rda"))

source(mypath("R", "building_dataset.R"))

###############
#  Profiling  #
###############

profvis::profvis({
  output <- lot_testing %>%
    dplyr::mutate(
      fish = purrr::pmap(
    list(
      id = lop_id,
      type = type_lot,
      min_size = lop_longueur_specimens_taille_mini,
      max_size = lop_longueur_specimens_taille_maxi,
      nb = lop_effectif
      ),
    gen_fish_from_lot, # Arguments from ind_measure:
    ind_measure = ind_measure_testing,
    ind_size = mei_taille,
    ind_id = mei_lop_id,
    verbose = TRUE)
    )
})

###########################
#  Test byte compilation  #
###########################

library('microbenchmark')

test_compile <- function (bytecode = TRUE) {
  if (bytecode) {
    fun <- compiler::cmpfun(gen_fish_from_lot)
  } else {
    fun <- gen_fish_from_lot
  }
  output <- lot_testing %>%
    dplyr::mutate(
      fish = purrr::pmap(
    list(
      id = lop_id,
      type = type_lot,
      min_size = lop_longueur_specimens_taille_mini,
      max_size = lop_longueur_specimens_taille_maxi,
      nb = lop_effectif
      ),
    fun, # Arguments from ind_measure:
    ind_measure = ind_measure_testing,
    ind_size = mei_taille,
    ind_id = mei_lop_id,
    verbose = TRUE)
    )
}
microbenchmark(test_compile(TRUE), test_compile(FALSE), times = 10L)

#################
#  Plan timing  #
#################

test_timing <- function (n = 100) {

  fun <- gen_fish_from_lot
  lot_testing <- sample_n(fish_lot, n)
  ind_measure_testing <- filter(lot_measure, mei_lop_id %in% lot_testing$lop_id)

  output <- lot_testing %>%
    dplyr::mutate(
      fish = purrr::pmap(
    list(
      id = lop_id,
      type = type_lot,
      min_size = lop_longueur_specimens_taille_mini,
      max_size = lop_longueur_specimens_taille_maxi,
      nb = lop_effectif
      ),
    fun, # Arguments from ind_measure:
    ind_measure = ind_measure_testing,
    ind_size = mei_taille,
    ind_id = mei_lop_id,
    verbose = TRUE)
    )
}

load(mypath("data-raw","fishing_op_build", "fish_lot.rda"))
load(mypath("data-raw","fishing_op_build", "lot_measure.rda"))


timing <- microbenchmark(test_timing(1000), test_timing(2000), test_timing(4000), test_timing(8000), times = 5)

timing2 <- tibble(
  n = timing$expr %>%
    str_replace_all("test_timing|\\(|\\)", "") %>%
    as.numeric,
  time = timing$time * 10^-9
  )

ggplot(timing2, aes(y = time, x = n)) +
  geom_point()
model <- lm(time ~ n, timing2)
newdata <- data.frame(
  n = c(100000, 500000, 1000000, 2000000),
  time = predict(model, newdata) / 3600
)

#######################
#  mask versus which  #
#######################


library('microbenchmark')
library('tibble')
seq_a <- tibble(
  id = seq(1, 2*10^6),
  v = id
  )
test_mask <- function (type = "which", x = seq_a) {
  if (type == "which") {
    out <- x[which(x$id == 1), ][["v"]]
  } else if (type == "mask"){
    out <- x[x$id == 1, ][["v"]]
  }
  out
}
microbenchmark(test_mask("which"), test_mask("mask"), times = 100)

###############
#  Check lot  #
###############

fish_lot %>%
  summarise_all(list(~sum(is.na(.)))) %>%
  unlist()
str(fish_lot)
unique(fish_lot$type_lot)
lot_measure %>%
  summarise_all(list(~sum(is.na(.)))) %>%
  unlist


# Type "S/L" and NA in ind measure
sl_lot <- filter(fish_lot, type_lot == "S/L") %>% .$lop_id 
filter(lot_measure, mei_lop_id %in% sl_lot) %>%
  summarise_all(list(~sum(is.na(.)))) %>%
  unlist
#ok

# lot not in lot_measure 
sum(!(lot_measure$mei_lop_id %in% fish_lot$lop_id))
#ok
