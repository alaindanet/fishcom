context("filter_dbl_station")

op <- tibble(
  station = rep(1, 3),
  times = today() - c(0, 370, 30),
  nb_sp = c(6, 6, 10),
  nb_ind = c(60, 60, 60)
)
int_op <- op %>%
  arrange(times) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, times[-1] - times[-length(station)])
  )
test  <- int_op %>%
  group_by(station) %>%
  nest


test_that("multiplication works", {
  keep_most_complete_sampling(test$data[[1]])

})
