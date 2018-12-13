context("metaweb_build")

library('tidyverse')
library('magrittr')

test_that("spliting size in classes works", {
  fake <- tibble(
    species = rep(c("Pikachu", "Salameche"), each = 10),
    size = c(seq(1,10), seq(101,110)))

  split_test <- split_in_classes(seq(1, 10), nb_class = 4)
  expect_equal(split_test, round(seq(0, 10, by = 10/9)))
  split_test <- split_in_classes(seq(1, 10), nb_class = 9, class_method = "quantile")
  expect_equal(split_test, round(quantile(seq(1, 10),
	probs = seq(0, 1, by = 1 / 9))))

  compute_classes(fake, species, size, nb_class = 4)
})
