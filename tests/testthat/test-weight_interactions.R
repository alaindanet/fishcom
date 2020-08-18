
sp_names <- c("bernard", "ermite", "crevette")
pred_mat <- matrix(
  c(rep(0, 3), c(0, 1, 0), rep(1, 3)),
  byrow = FALSE, ncol = 3, dimnames = list(sp_names, sp_names))

pred_bm_demand <- 
  tibble(sp_class = colnames(pred_mat),
    nind = c(1, 10, 100),
    bm_prod = c(100, 10, 1)
    ) %>%
  sample_n(3)


test_that("weighting works", {

expected_mat <- matrix(
  c(rep(0, 3),
    c(0, 10, 0),
    c(1 * 1/111, 10/111, 100/111)),
  ncol = 3,
  byrow = FALSE,
  dimnames = list(sp_names, sp_names)
) 

expect_identical(
  expected_mat,
  get_weighted_fish_fish_adj_matrix(
    .data = pred_bm_demand,
    network = pred_mat  
  )
)

})
