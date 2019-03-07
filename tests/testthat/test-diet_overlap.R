context("diet_overlap")

test_that("overlap return the good values", {
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

test_that("Overlap for matrix works", {
  sp1 <- c(0, 1, 0, 1)
  sp2 <- c(1, 0, 1, 0)
  sp3 <- c(1, 1, 1, 0)
  adj <- cbind(sp1, sp2, sp3)

  res <- compute_diet_overlap(adj)
  expected <- c(Osp1_sp2 = 0, Osp1_sp3 = .41, Osp2_sp3 = .82)
  expect_equal(round(res, 2), expected)
})
