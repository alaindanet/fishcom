#################
#  Map station  #
#################


library(tidyverse)
library(magrittr)
library(sf)
library(raster)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "plot_methods.R"))
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

# Elevation
elev <- raster(mypath("data-raw", "dem_250m_lambert_93.tif"))
elev_1km <- aggregate(elev, fact=4)
relief_spdf <- as(elev_1km, "SpatialPixelsDataFrame")
get_hill_shade_df <- function(dem = NULL,
  z = NULL, fact_aggr = NULL,  angle = 40, direction = 135) {

  if (!is.null(fact_aggr)) {
    dem <- aggregate(dem, fact = fact_aggr)
  }

  if (!is.null(z)) {
    dem <- dem * z 
  }

  slope_aspect <- purrr::map(list(slope = "slope", aspect = "aspect"),
    ~terrain(dem, opt =.x, unit='radians')
  )
  hillshade <- hillShade(
    slope_aspect$slope,
    slope_aspect$aspect,
    angle = angle,
    direction = direction
  )

  output <- purrr::map(
    list(
      slope = slope_aspect$slope,
      aspect = slope_aspect$aspect,
      hillshade = hillshade
      ), function (x) {

      spdf <- as(x, "SpatialPixelsDataFrame")
      out <- as.data.frame(spdf)
      colnames(out)[1] <- "value" 
      return(out)
    }) 

  return(output)
}
plot(hillshade$hillshade)
head(hillshade$slope)
plot(hillshade, col=grey.colors(100, start=0, end=1), legend=F, alpha=.8, add=TRUE)
plot(slope, col=grey.colors(100, start=1, end=0), legend=F, alpha=.2, add=TRUE)


# Basin data
basin <- read_sf(mypath("data-raw", "basin_dce", "BassinDCE.shp"))
basin %<>% st_transform(crs = 2154)
basin %<>% filter(CdBassinDC %in% c("B1", "B2", "A", "D", "F", "C", "G", "H"))
basin <- rmapshaper::ms_simplify(input = basin) %>%
  st_as_sf()

# Station
myload(biomass_ts_sax, op_analysis, dir = mypath("data"))
op_analysis_bbb <- filter(op_analysis, station %in%
  biomass_ts_sax[biomass_ts_sax$sax == "bbb",]$station)
station <- get_basin_station(sf_obj = TRUE)
station %<>%
  filter(station %in% unique(op_analysis$station))
## Get bbb
station %<>%
  mutate(type = ifelse(
      station %in% unique(op_analysis_bbb$station),
      "stable", "unstable")
  )


#crop elev
crop_elev <- crop(
  x = elev_1km,
  y = extent(basin))
hillshade <- get_hill_shade_df(
  dem = crop_elev,
  z = 100,
  fact_aggr = NULL 
  )$hillshade


# Plot
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library(cowplot)
library(ggspatial)
library("rnaturalearth")
library("rnaturalearthdata")

# World and country
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 2154)
country_points <- st_centroid(world) %>%
  filter(name %in% c("France", "Italy", "Spain", "Belgium", "Germany", "Switzerland"))
country_points <- cbind(country_points, st_coordinates(country_points$geometry))
country_points[country_points$name == "Germany", c("X", "Y")] <- c(1200000, 7000000) 
country_points[country_points$name == "Spain", c("X", "Y")] <- c(300000, 6150000) 
country_points[country_points$name == "Italy", c("X", "Y")] <- c(1300000, 6450000) 

ocean <- data.frame(X = 200000, Y = 6500000, name = "atlantic \n ocean")
sea <- data.frame(X = c(450000, 1000000), Y = c(7025000, 6150000), name = c("Manche \n sea", "Mediterranean \n sea"))

basin_inner <- basin %>% 
        rmapshaper::ms_innerlines() %>% 
        as_tibble() %>% 
        st_as_sf()
  #
    
p <- ggplot() +
  geom_sf(data = st_geometry(world)) +
  # Coutry
  geom_text(data= country_points,aes(x=X, y=Y, label=toupper(name)),
    color = "black", fontface = "bold", check_overlap = FALSE) +
  # Ocean + sea
  geom_text(data= ocean,aes(x=X, y=Y, label=toupper(name)),
    color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  geom_text(data= sea,aes(x=X, y=Y, label=name),
    color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  #theme_map() +
    # Plot basin
    geom_sf(data = st_geometry(basin_inner)) +
    # raster comes as the first layer, municipalities on top
    geom_raster(data = hillshade,
      aes(x = x, y = y, alpha = value)) +
    ## use the "alpha hack"
    scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    # Add station
    geom_sf(data = station, aes(color = as.factor(type))) + 
    scale_color_manual(values = c("stable" = "black", "unstable"= "darkgrey")) +
    coord_sf(
      xlim = c(60000, 1100000),
      ylim = c(6100000, 7122216),
      datum = sf::st_crs(2154),
      crs = 2154,
      expand = FALSE
    )

p_st <- p +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.75, "in"),
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.25) +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
    panel.background = element_rect(fill = "aliceblue"))
  #Plot europe

mysave(p_st, dir = mypath("manuscript", "bef_stability", "figs"))
save_plot(
  filename = mypath("manuscript", "bef_stability", "figs", "map.pdf"),
  plot = p_st#,
  #base_height = ,
  #base_width = 
)
