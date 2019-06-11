################################################################################
#                               Extract results                                #
################################################################################


mypath <- rprojroot::find_package_root_file
Sys.setenv(LANG = "en")
mydir <- mypath("data-raw")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))
library('magrittr')
library('tidyverse')

myload(flow_interpolated, dir = mypath("data-raw", "ssn_interpolation"))
flow_interpolated %<>%
  select(-model, - data)

flow_prediction <- flow_interpolated %>%
  unnest(prediction)
mysave(flow_prediction, dir = mypath("data-raw", "ssn_interpolation"))

myload(flow_prediction, dir = mypath("data-raw", "ssn_interpolation"))
flow_prediction %>%
  arrange(desc(avg_data))
test <- flow_prediction %>%
  filter(id == 5231)

ggplot(test, aes(x = year, y = avg_data)) +
  geom_line() +
  geom_point()

