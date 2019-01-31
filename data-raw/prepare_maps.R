################################################################################
#                     Prepare maps for efficient plotting                      #
################################################################################

library(tidyverse)
library(magrittr)
library(sf)
library(rmapshaper)

region <- read_sf("france_region_shp/regions-20180101.shp") %>%
  dplyr::filter(surf_km2 > 10000, nom != "Guyane")

# Simplify polygons
#https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
simplepolys <- rmapshaper::ms_simplify(input = region, tol) %>% #Can be used also for lines
  st_as_sf()
plot(simplepolys[, "nom"])

region_polygon <- select(simplepolys, nom)
devtools::use_data(region_polygon, overwrite = TRUE)

## To continue with
#- cours_eau_shp
#- obstacles
#- station
# Change projection:
#https://www.rdocumentation.org/packages/sf/versions/0.7-2/topics/st_transform
