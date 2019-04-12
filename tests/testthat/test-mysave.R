context("test-mysave")

mypath <- rprojroot::find_package_root_file
library('datasets')
data(cars)

test_that("mysave works", {
  expect_message(
    mysave(cars, dir = mypath("data"))
  )
  temp <- mypath("data", "cars.rda")
  expect_equal(
    file.exists(temp), TRUE
  )

  expect_error(
    mysave(cars, dir = mypath("data"))
  )
  expect_message(
    mysave(cars, dir = mypath("data"), overwrite = TRUE)
  )

  if (file.exists(temp)) {
    file.remove(temp)
  }

})
