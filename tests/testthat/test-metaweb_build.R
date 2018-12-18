context("metaweb_build")

library('tidyverse')
library('magrittr')

data(fish_length_toy)
data(fish_diet_shift)
data(resource_diet_shift)
data(pred_win)
#############
#  Metaweb  #
#############
test_that("Metabuild returns a correct matrix",{
  fish_diet_shift %<>% filter(species %in% fish_length_toy$species)

  metaweb <- build_metaweb(fish_length_toy, species, length, pred_win, fish_diet_shift, resource_diet_shift)

  expect_is(metaweb, "matrix")

  matrix_to_rep <- read_csv2("../../bonnafe_work/data/bib_data/output/AccMat_quant_9_PWvar_partOverlap.csv") %>% as.matrix

  expect_identical(metaweb, matrix_to_rep)

})

#######################
#  Size distribution  #
#######################
nb_class <- 9
min_size <- 1
max_size <- 10
test_that("spliting size in classes works", {

  # Percentile method
  split_test <- split_in_classes(seq(min_size, max_size), nb_class = nb_class)
  expected_split <- round(seq(0, max_size, by = max_size / nb_class))

  expected_df <- tibble(
    class_id = seq(1, nb_class),
    lower = expected_split[-length(expected_split)],
    upper = expected_split[-1]
    )
  expect_identical(split_test, expected_df)

  # Quantile method
  split_test <- split_in_classes(seq(min_size, max_size), nb_class = nb_class, class_method = "quantile")
  expected_split <- round(quantile(seq(min_size, max_size),
	probs = seq(0, 1, by = 1 / nb_class)))

  expected_df <- tibble(
    class_id = seq(1, nb_class),
    lower = expected_split[-length(expected_split)],
    upper = expected_split[-1]
    )
  expect_identical(split_test, expected_df)

})

fake <- tibble(
  species = rep(c("Pikachu", "Salameche"), each = 10),
  size = c(seq(1,10), seq(101,110)))
# Compute_classes
classes_species <- compute_classes(fake, species, size, nb_class = nb_class)
test_that("Class dataframe is correct", {
  pika_class <- split_in_classes(unlist(filter(fake, species == "Pikachu")$size))
  sala_class <- split_in_classes(unlist(filter(fake, species == "Salameche")$size))

  expected_df <- tibble(
    species = rep(c("Pikachu", "Salameche"), each = nb_class),
    class_id = rep(seq(1, nb_class), 2),
    lower = c(unlist(pika_class$lower), unlist(sala_class$lower)),
    upper = c(unlist(pika_class$upper), unlist(sala_class$upper))
    )
  expect_identical(classes_species, expected_df)

  classes_species <- compute_classes(fish_length_toy, species, length, nb_class = nb_class)
  fish_length_toy[1, "length"] <- NA
  # Test success:
  expect_error(compute_classes(fish_length_toy, species, length, nb_class = nb_class, na.rm = TRUE), NA)
  # Test error:
  expect_error(compute_classes(fish_length_toy, species, length, nb_class = nb_class), message = "There are NAs in your dataset. Please set na.rm = TRUE")
  
})

#######################
#  Compute prey size  #
#######################
fake_prey_win <- distinct(fake, species) %>%
  select(species) %>%
  mutate(
    beta_min = .03,
    beta_max = .45,
    )
th_prey_size <- compute_prey_size(classes_species, fake_prey_win, species, beta_min, beta_max, pred_win_method = "midpoint")
test_that("We get a correct prey size dataframe", {

  expect_is(th_prey_size, "data.frame")
  expected_prey_size <- classes_species %>%
    mutate(
    min = 0.03 * ( (lower + upper) / 2),
    max = 0.45 * ( (lower + upper) / 2)
    ) %>% select(-lower, -upper)

  expect_identical(th_prey_size, expected_prey_size)

})

#######################
#  Compute piscivory  #
#######################

fake_onto_diet_shift <- tibble(
    species = rep(c("Pikachu", "Salameche"), each = 2),
    life_stage = rep(c(1, 2), each = 2),
    min = c(0, 5, 0, 105),
    max = c(5, Inf, 105, Inf)
    )

