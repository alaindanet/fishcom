################################################################################
#                     Exploration of interpolation results                     #
################################################################################

library(tidyverse)
library('lubridate')
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

# Load fish and environmental station

res <- lapply(c("ouest", "nord", "sud", "est"), function (basin) {
  myload(quality_prediction, dir = mypath("data-raw", "ssn_interpolation", basin))
  quality_prediction
})
quality <- do.call(rbind, res)

cross_val <- quality %>%
  select(-prediction) %>%
  mutate(cross_v = map(cross_v, enframe)) %>%
  unnest()

# Prediction:
prediction <- quality %>%
  select(-cross_v) %>%
  unnest(prediction)
press <- prediction %>%
  group_by(id, var_code) %>%
  summarise(press = mean(avg_data))

mysave(cross_val, prediction, press, dir = mypath("data-raw", "ssn_interpolation"), overwrite = TRUE)
