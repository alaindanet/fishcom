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
  left_join(., dplyr::select(op_analysis, opcod, station, date)) 

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
  nrow_sp_legend = 2,
  return_data = TRUE
)
legends <- cowplot::get_legend(
    p$plots[[1]] +
      scale_color_manual(values = p$color, limits = names(p$color)) +
      labs(colour = "Species", size = "Biomass") +
      guides(
	colour = guide_legend(
	  label.position = "left",
	  ncol = 4,
	  byrow = TRUE,
	  title.position = "top"),
	size = guide_legend(
	  nrow = 1,
	  byrow = TRUE,
	  title.position = "top"
	)
	) +
      theme(
	plot.margin = unit(c(0, 0, 0, 0), "cm"),
	legend.direction = "horizontal", 
	legend.position = "bottom",
	legend.box = "vertical",
	legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
	legend.spacing.x = unit(.1, 'cm'),
	legend.spacing.y = unit(.1, 'cm'),
	text = element_text(size = 8),
	legend.text = element_text(
	  size = NULL,
	  margin = margin(r = 5)
	)
      )
  )
plots <- map(p$plots, function (x) {
  x + theme(legend.position = "none",
    plot.margin = unit(c(1, 0, 0, 0), "cm"))
  }) 

p_tmp_net <- plot_grid(plotlist = plots[1:4])
temporal_network <- plot_grid(
  p_tmp_net,
  legends,
  #ncol = 2, rel_widths= c(1, .4)
  nrow = 2, rel_heights= c(1, .5)
)
temporal_network

save_plot(
  filename = mypath("manuscript", "bef_stability", "figs", "temporal_network.pdf"),
  temporal_network)
mysave(temporal_network, dir = mypath("manuscript/bef_stability/figs"), overwrite = TRUE)

######
# metaweb   #
######

library(igraph)
metaweb <- meta$metaweb
meta
g <- igraph::graph_from_adjacency_matrix(metaweb, mode = "directed")

str(V(g))

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
