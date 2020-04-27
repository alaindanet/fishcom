library(tidyverse)
library(magrittr)
library(cowplot)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "plot_methods.R"))

myload(network_analysis, dir = mypath("data", "classes"))
myload(op_analysis, metaweb_analysis, dir = mypath("data"))
meta <- metaweb_analysis; rm(metaweb_analysis)

st_sp_node <- network_analysis %>%
  select(opcod, composition) %>%
  left_join(dplyr::select(op_analysis, opcod, station, date), by = "opcod") %>%
  unnest(cols = c(composition)) %>%
  group_by(station) %>%
  summarise(
    nbnode = length(unique(sp_class)),
    nbsp = length(unique(str_extract(sp_class, "[A-Z]+")))
  ) %>%
  arrange(nbsp)


net <- network_analysis %>%
  dplyr::select(opcod, network) %>%
  unnest(cols = c(network)) %>%

station_to_get <- function (rich = NULL)  {
  tmp <- filter(st_sp_node, nbsp == rich)
  choice <- sample(tmp$station, 1)
  cat(choice, "\n")
  return(choice)
}
choice <- station_to_get(6)

net_ex <- filter(net, station == 11329) %>% #choice
  mutate(date = lubridate::year(date)) %>%
  filter(date %in% seq(2000, 2004))

p <- my_crap_temporal_network(
  net = net_ex,
  metaweb = meta$metaweb,
  network_data = network_analysis,
  nrow_sp_legend = 1,
)
p
save_plot(filename = mypath("manuscript", "bef_stability", "figs", "temporal_network.pdf"),
  p)
temporal_network <- p
mysave(temporal_network, dir = mypath("manuscript/bef_stability/figs"), overwrite = TRUE)

######
# metaweb   #
######

metaweb <- meta$metaweb


llay <- ggraph::create_layout(metaweb, layout = "kk")
node_pos <- NetIndices::TrophInd(metaweb, Dead = NULL)$TL - 1
llay$y <- node_pos
llay$species <- get_species(llay$name)

p <- ggraph::ggraph(llay) +
  ggraph::geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  coord_cartesian(ylim=c(1,4.5))
save_plot(
filename = mypath("manuscript/bef_stability/figs", "metaweb.pdf"),
p
)
#p #Too much time to compute
