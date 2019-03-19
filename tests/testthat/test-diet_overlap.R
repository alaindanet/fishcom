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

sp1 <- c(0, 1, 0, 1)
sp2 <- c(1, 0, 1, 0)
sp3 <- c(1, 1, 1, 0)
adj <- cbind(sp1, sp2, sp3)

res <- compute_diet_overlap(adj)

test_that("Overlap for matrix works", {
  expected <- c(Osp1_sp2 = 0, Osp1_sp3 = .41, Osp2_sp3 = .82)
  expected_mat <- matrix(1, nrow = ncol(adj), ncol = ncol(adj),
    dimnames = list(colnames(adj), colnames(adj)))
  expected_mat[upper.tri(expected_mat, diag = FALSE)] <- expected 
  expected_mat[lower.tri(expected_mat, diag = FALSE)] <-
    expected_mat[upper.tri(expected_mat, diag = FALSE)]

  expect_equal(round(res, 2), expected_mat)
})

test_that("We can get average overlap", {

  diag(res) <- NA
  newres <- matrix(
    res[which(!is.na(res))],
    nrow = nrow(res) - 1,
    ncol = ncol(res),
    byrow = FALSE
  )
  colnames(newres) <- colnames(res)

  expect_equal(colSums(newres), average_species_overlap(res))

})
