
######################
#  Temporal network  #
######################

library(tidyverse)
library(magrittr)
#library(igraph)
library(ggraph)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "plot_methods.R"))
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

myload(network_analysis, network_metrics, dir = mypath("data", "classes"))
myload(op_analysis, metaweb_analysis, dir = mypath("data"))
meta <- metaweb_analysis; rm(metaweb_analysis)

net <- network_analysis %>%
  dplyr::select(opcod, network) %>%
  unnest() %>%
  left_join(., dplyr::select(op_analysis, opcod, station, date)) 

network_metrics %>%
  dplyr::select(opcod, composition) %>%
  filter(opcod %in% net$opcod) %>%
  unnest()

test <- network_metrics %>%
  dplyr::select(opcod, obs_troph_level) %>%
  mutate(obs_troph_level = map(obs_troph_level, enframe)) %>% 
  unnest()
test %>%
  ggplot(aes(x = value)) +
  geom_histogram()


net_ex <- filter(net, station == sample(net$station, 1))

my_crap_temporal_network(net = net_ex, metaweb = meta$metaweb, network_data
  = network_analysis)


