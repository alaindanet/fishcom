context("filter_dbl_station")
library('lubridate')

op <- tibble(
  station = rep(1, 3),
  date = today() - c(0, 370, 30),
  nb_sp = c(6, 6, 10),
  nb_ind = c(60, 60, 60)
)
int_op <- op %>%
  arrange(date) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, date[-1] - date[-length(station)])
  )
test  <- int_op %>%
  group_by(station) %>%
  nest


test_that("sanatize works", {
 res <-  keep_most_complete_sampling(test$data[[1]])
 expected <- tibble(
  date = today() - c(370, 30),
  nb_sp = c(6, 10),
  nb_ind = c(60, 60),
  point = c(1, 2) %>% as.integer,
  sample_sep = c(NA, 340)
)
 expect_identical(res, expected)

})

op <- tibble(
  station = rep(1, 3),
  date = today() - c(0, 370, 30),
  nb_sp = c(NA, NA, NA),
  nb_ind = c(NA, NA, NA)
)
int_op <- op %>%
  arrange(date) %>%
  mutate(
    point = seq(1, length(station)),
    sample_sep = c(NA, date[-1] - date[-length(station)])
  )
test  <- int_op %>%
  group_by(station) %>%
  nest

test_that("sanatize handle NAs", {
 res <-  keep_most_complete_sampling(test$data[[1]])
 expected <- tibble(
  date = today() - c(370, 30),
  nb_sp = c(NA, NA),
  nb_ind = c(NA, NA),
  point = c(1, 2) %>% as.integer,
  sample_sep = c(NA, 340)
)
 expect_identical(res, expected)

})

