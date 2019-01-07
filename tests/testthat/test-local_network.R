context("local_network")

library(tidyverse)
library(magrittr)

nb_class <- 9
min_size <- 1
max_size <- 10

fake <- tibble(
  species = rep(c("Pikachu", "Salameche"), each = 10),
  size = c(seq(1,10), seq(101,110)))
# Compute_classes
classes_species <- compute_classes(fake, species, size, nb_class = nb_class)

test_that("class assignation works", {

  # Quantile method
  expected_vector <- as.integer(c(1, 1, seq.int(2, 9)))
  expect_equal(get_size_class(fake, Pikachu, size, classes_species) %>% as.numeric(.), expected_vector)

})


fake_station <- tibble(
  station = rep(rep(c(1, 2, 3, 4, 5), each = 2), times = 2),
  species = rep(c("Pikachu", "Salameche"), each = 10),
  size = c(seq(1, 10), 110, seq(101, 109)))

test_that("class assignation works with several species", {

  expected_df <- tibble(
    station = rep(rep(c(1, 2, 3, 4, 5), each = 2), times = 2),
    species = rep(c("Pikachu", "Salameche"), each = 10),
    class_id = c(c(1, 1, seq.int(2, 9)), c(9, 1, 1, seq.int(2, 8))) %>% as.integer(.)
    )
  expect_equal(assign_size_class(fake_station, species, size, classes_species), expected_df)

})
