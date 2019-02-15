context("nestedness")

library(magrittr)

test_that("nestedness returns a correct matrix", {
  data(test_network)

  output <- nestedness(test_network)
  # Expectation:
  species_comb <- combn(colnames(test_network), 2) %>% as.data.frame %>% t
  nij       <- c(1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3) %>% sum
  min_ni_nj <- c(1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 3) %>% sum
  nestedness_expectation <- nij / min_ni_nj
  #names(nestedness_expectation) <- "nestedness"
  expect_equal(output, nestedness_expectation)

  #Dummy tests:
  test_network[which(test_network != 0)] <- 0
  expect_equal(nestedness(test_network), NaN)

  test_network[which(test_network != 1)] <- 1
  expect_equal(nestedness(test_network), 1)
})
