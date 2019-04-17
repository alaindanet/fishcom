context("test-mysave")

mypath <- rprojroot::find_package_root_file
library('datasets')
data(cars)

data_dir <- mypath("data")

test_that("mysave works", {
  expect_message(
    mysave(cars, dir = data_dir)
  )
  temp <- mypath("data", "cars.rda")
  expect_equal(
    file.exists(temp), TRUE
  )

  expect_error(
    mysave(cars, dir = data_dir)
  )
  expect_message(
    mysave(cars, dir = data_dir, overwrite = TRUE)
  )

  myload(cars, dir = data_dir)

  if (file.exists(temp)) {
    file.remove(temp)
  }

})
