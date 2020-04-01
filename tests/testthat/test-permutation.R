# Exact test

x <- rnorm(100, 0, 2)
y <- rnorm(100, 2, 2)


test_that("multiplication works", {
  expect_true(permutation_test(x, y, 10000)[["p_val"]] < 0.05)
})
