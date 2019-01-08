context("metaweb_build")

library('tidyverse')
library('magrittr')


#######################
#  Size distribution  #
#######################
nb_class <- 9
min_size <- 1
max_size <- 10
test_that("spliting size in classes works", {

  # Percentile method
  split_test <- split_in_classes(seq(min_size, max_size), nb_class = nb_class)
  expected_split <- round(seq(min_size, max_size, by = (max_size - min_size) / nb_class))

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
test_that("Remove species when revelant", {

ten_one <- tibble(
  species = rep(c("Pikachu", "Salameche"), each = 10),
  size = c(seq(1, 10), rep(1, 10))
  )

expect_warning(compute_classes(ten_one, species, size, nb_class = nb_class), message = "The following species had less than two unique size values, so we got rid of them: Salameche")

not_one <- tibble(
  species = rep("Salameche", each = 10),
  size = c(rep(1, 10))
  )
expect_error(compute_classes(not_one, species, size, nb_class = nb_class), message = "None of the species got more of two unique values. Check your dataset.")

one <- tibble(
  species = rep("Salameche", each = 1),
  size = c(rep(1, 1))
  )

expect_error(compute_classes(one, species, size, nb_class = nb_class), message = "None of the species got more of two unique values. Check your dataset.")
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
    min_prey = 0.03 * ( (lower + upper) / 2),
    max_prey = 0.45 * ( (lower + upper) / 2)
    ) %>% select(-lower, -upper)

  expect_identical(th_prey_size, expected_prey_size)

})

#######################
#  Compute piscivory  #
#######################
fake_onto_diet_shift <- tibble(
    species = rep(c("Pikachu", "Salameche"), each = 2),
    life_stage = rep(c(1, 2), times = 2),
    min = c(0, 3, 0, 102),
    max = c(3, Inf, 102, Inf),
    light = c(0, 0, 0, 0),
    Chetiflor = c(0, 1, 1, 0),
    Paras = c(0, 0, 1, 1),
    pisc = rep(c(0, 1), times = 2)
    )
test_that("piscivory is well computed", {
   piscivory_table <- compute_piscivory(classes_species, fake_onto_diet_shift, species = species, low_bound = min, upper_bound = max, fish = pisc)
   expected_table <- th_prey_size %>%
     mutate(pisc_index = c(rep(0, 2), rep(1, 7), 0, rep(1, 8))) %>%
     select(-min_prey, -max_prey)
   expect_identical(piscivory_table, expected_table)
    })
test_that("piscivory is sensible to variable definition", {
fake_onto_diet_shift$min <- c(0, "max", 0, 102) 

expect_error(compute_piscivory(classes_species, fake_onto_diet_shift, species = species, low_bound = min, upper_bound = max, fish = pisc), "In fish_diet_shift, the low_bound variable is not numeric")
    })
test_that("it returns piscivory index non-piscivorous fishes", {
# Salameche does not eat fishes 
fake_onto_diet_shift$pisc <- c(0, 1, 0, 0)

   expected_table <- th_prey_size %>%
     mutate(pisc_index = c(rep(0, 2), rep(1, 7), 0, rep(0, 8))) %>%
     select(-min_prey, -max_prey)
expect_identical(compute_piscivory(classes_species, fake_onto_diet_shift, species = species, low_bound = min, upper_bound = max, fish = pisc), expected_table)
    }
  )

#############
#  Metaweb  #
#############
fake_resource_shift <- tibble(
    species = c("Chetiflor", "Paras"),
    life_stage = c(0, 0),
    min = c(0, 0),
    max = c(0, 0),
    light = c(1, 0),
    Chetiflor = c(0, 1),
    Paras = c(0, 0),
    pisc = c(0, 0)
    )
test_that("Metabuild returns a correct matrix",{

  metaweb <- build_metaweb(fake, species, size = size, fake_prey_win, fake_onto_diet_shift, min, max, fish = pisc, fake_resource_shift)

  expect_is(metaweb$metaweb, "matrix")

    })
test_that("metaweb works on a true dataset", {
  ## TRUE dataset
  data(fish_length)
  data(fish_diet_shift)
  data(resource_diet_shift)
  data(pred_win)
  #fish_diet_shift %<>% filter(species %in% fish_length_toy$species)
  #pred_win %<>% filter(species %in% fish_length_toy$species)
  matrix_to_rep <- read_csv2("../../bonnafe_work/data/bib_data/output/AccMat_quant_9_PWvar_partOverlap.csv") %>% as.matrix
  ## Remove OBL which has only one record:
  if (filter(fish_length, species == "OBL") %>% nrow <= 1) {
    col_to_rm <- str_detect(colnames(matrix_to_rep), "OBL")
    matrix_to_rep2 <- matrix_to_rep[- which(col_to_rm) , - which(col_to_rm)]
  }
# Order matrix:
order_species_to_rep <- str_extract_all(colnames(matrix_to_rep2), "[A-Za-z]+", simplify = TRUE) %>% as.vector

  metaweb <- build_metaweb(fish_length, species, length, pred_win, fish_diet_shift, size_min, size_max, fish, resource_diet_shift, na.rm = TRUE)
col_species <- str_extract_all(colnames(metaweb$metaweb), "[A-Za-z]+", simplify = TRUE) %>% as.vector

metaweb2 <- metaweb$metaweb[order(match(col_species, order_species_to_rep)), order(match(col_species, order_species_to_rep))]
colnames(matrix_to_rep2) <- colnames(metaweb2)
rownames(matrix_to_rep2) <- colnames(metaweb2)

  expect_identical(metaweb2, matrix_to_rep2)
#HERE: replace properly dimnames to see where are the differences between the
##two matrices
attr(metaweb2, "dimnames")  <- list(order_species_to_rep, order_species_to_rep)
attr(matrix_to_rep2, "dimnames")  <- list(order_species_to_rep, order_species_to_rep)

# There is problems with fish fish interactions:
metaweb2[37]; dimnames(metaweb2)[[2]][37]; dimnames(metaweb2)[[1]][1]
metaweb2[46]
sum(matrix_to_rep2-metaweb2)

})
