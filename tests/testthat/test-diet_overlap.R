context("diet_overlap")

test_that("multiplication works", {
  sp1 <- c(0, 1, 0, 1)
  sp2 <- c(1, 0, 1, 0)

  res <- compute_two_species_overlap(sp1, sp2)
  expect_equal(res, 0)

  sp1 <- c(1, 1, 1, 1)
  sp2 <- c(1, 1, 1, 1)
  
  res <- compute_two_species_overlap(sp1, sp2)
  expect_equal(res, 1)

  sp1 <- c(1, 0, 1, 0)
  sp2 <- c(1, 1, 1, 0)

  res <- compute_two_species_overlap(sp1, sp2)
  expect_equal(round(res, 2), .82)
})
