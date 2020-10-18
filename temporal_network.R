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
  left_join(., dplyr::select(op_analysis, opcod, station, date)) %>%
  mutate(date = lubridate::year(date)) 


station_to_get <- function (rich = NULL)  {
  tmp <- filter(st_sp_node, nbsp == rich)
  choice <- sample(tmp$station, 1)
  cat(choice, "\n")
  return(choice)
}
choice <- station_to_get(6)

# Choose two stations: stable / not stable 
myload(sem_data, dir = mypath("data"))
quant <- quantile(sem_data$bm_std_stab, na.rm = TRUE)[c("25%", "75%")]
# the two stations closest to first and third quantile
qt_station <- sem_data %>%
  select(station, bm_std_stab) %>%
  slice(which.min(abs(bm_std_stab - quant[1])), which.min(abs(bm_std_stab - quant[2])))


net_ex <- filter(net, station == qt_station$station[1])

net_temp <- net_ex %>%
  dplyr::select(date, from, to) %>%
    arrange(date) %>%
    group_by(date) %>%
    nest(.key = "network") %>%
    mutate(
      igraph_obj = map(network, igraph::graph_from_data_frame,
        directed = TRUE),
      adj_mat = map(igraph_obj, igraph::as_adjacency_matrix,
        sparse = FALSE),
      troph = map(adj_mat, ~NetIndices::TrophInd(.x)),
      obs_troph_level_vector = map(troph, function (x) {
	#out <- x$TL
	#names(out) <- rownames(x)
	tibble(
	  TL = x$TL, 
	  name = rownames(x)
	)
	#return(out)
	}
      )
    )

#library(ggnetwork)
##install.packages("ggnetwork")
#test <- ggnetwork(net_temp$igraph_obj[[1]]) %>%
  #left_join(net_temp$obs_troph_level_vector[[1]] , by = "name") %>%
  #mutate(
    #TLend = TL + y - yend
  #)
#ggplot(test, aes(y = TL, x = x, xend = xend, yend = TLend)) +
  #geom_edges()
#ggplot(test, aes(y = y, x = x, xend = xend, yend = yend)) +
  #geom_edges() +
  #geom_nodes()

#plot_temporal_network

library(tidygraph)
library(ggraph)
unique(net_ex$date)
source(mypath("R", "plot_methods.R"))
#debugonce(my_crap_temporal_network)
p <- my_crap_temporal_network(
  net = net_ex,
  metaweb = meta$metaweb,
  network_data = network_analysis,
  nrow_sp_legend = 2,
  return_data = TRUE,
  my_y_lim = c(0, 4.1),
  bm_var = "bm_std"
)
names(p)
# Modify for biomass std 
# normal edge
# 

p2 <- my_crap_temporal_network(
  net = filter(net, station == qt_station$station[2]),
  metaweb = meta$metaweb,
  network_data = network_analysis,
  nrow_sp_legend = 2,
  return_data = TRUE,
  my_y_lim = c(0, 4.1),
  bm_var = "bm_std"
)

temporal_network <- plot_grid(plotlist = p$plots[-1], ncol = 4)
temporal_network2 <- plot_grid(plotlist = p2$plots, ncol = 4)


myload(p_st,temporal_network, final_temporal_network, dir = mypath("manuscript/bef_stability/figs"))

# Network inference
net_method_path <- mypath("manuscript/bef_stability/figs",
  "hypothesis.pdf")
metaweb_path <- mypath("manuscript/bef_stability/figs",
  "metaweb.png")

library(magick)
p_net_method <- cowplot::ggdraw() +
  cowplot::draw_image(magick::image_read_pdf(net_method_path))
p_metaweb <- cowplot::ggdraw() +
  cowplot::draw_image(magick::image_read(metaweb_path), hjust = 0) 

final_temporal_network <- plot_grid(
  temporal_network2,
  temporal_network,
  nrow = 2, rel_heights= c(1, 1),
  align = "none",
  labels = c("C", "D"),
  vjust = c(-.5, .5), hjust = 0
  )

fig1 <- plot_grid(
  plot_grid(p_st, p_metaweb, labels = "AUTO", vjust = 2, hjust = -0.5),
  final_temporal_network,  ncol = 1,
  rel_heights = c(.6, 1)
)

save_plot(
  filename = mypath("manuscript", "bef_stability", "figs", "fig1.pdf"),
  fig1, base_height = 10,
  base_asp = .6
)

save_plot(
  filename = mypath("manuscript", "bef_stability", "figs", "final_temporal_network.pdf"),
  final, base_height = 3, base_asp = 2)
mysave(final_temporal_network, dir = mypath("manuscript/bef_stability/figs"), overwrite = TRUE)


######
# metaweb   #
######

library(igraph)
metaweb <- meta$metaweb
meta
g <- igraph::graph_from_adjacency_matrix(metaweb, mode = "directed")

species_color <- set_color_species(
  node_list = names(V(g)),
  species_list = meta$species, 
  resource_list = meta$resource 
)

V(g)$color <- map_chr(
  str_extract(names(V(g)), "[A-Z]{3}|[a-z]+"),
  function(x){
  species_color[names(species_color) == x]
  }
)

# Define layout
lay <- layout.fruchterman.reingold(g)
lay <- layout_with_fr(g)
# Compute trophic level
dead_material <- c("det", "biof")
lay[, 2] <- NetIndices::TrophInd(metaweb, Dead = dead_material)$TL

png(
  filename = mypath("manuscript/bef_stability/figs",
    "metaweb.png"), 
  units = "in",
  width = 5,
  height = 5,
  pointsize = 12*96/72,
  res = 96 
)

V(g)$label <- NA
V(g)$size <- 20 
org_par <- par(mar = c(5, 4, 0, 0) + 0.1)
plot(g,
  layout = lay,
  edge.arrow.size=.2,
  edge.curved=0,
  #vertex.color = "orange",
  vertex.frame.color="#555555",
  vertex.label.color="black",
  rescale = FALSE,
  asp = 0,
  axes = FALSE, 
  ylim = c(1,4),
  xlim = c(-6.2,2.7),
  ylab = "Trophic level"
)
Axis(side=2, labels=TRUE)
legend(
  x      = -9.2,
  y      = 0.9,
  legend = names(species_color),
  pch    = 21,
  col    = "#777777",
  pt.bg  = species_color,
  pt.cex = 0.7,
  cex    = .5,
  bty    = "n",
  ncol   = 7 
)
par(org_par)
dev.off()
