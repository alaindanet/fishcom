################################################################################
#                    Prepare data for flow and quality data                    #
################################################################################

library('tidyverse')
library('magrittr')
library('sf')
library('rgeos')
library('dbplyr')
#devtools::load_all()
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "flow_quality")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

donuts <- read_sf(paste0(mydir, "/stations_FR_river_donuts.shp"))
donuts <- st_transform(donuts, crs = 4326)
st_crs(donuts)

myload(station, dir = mypath("data-raw"))
myload(op_analysis_complete_partial, dir = mypath("data"))
station <- st_as_sf(station, coords = c("lon", "lat"),  crs = 4326) %>%
  filter(id %in% op_analysis_complete_partial$station)

myload(region_polygon, dir = mypath("data"))

plot(st_geometry(region_polygon))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
plot(st_geometry(station), add = TRUE, pch = 20, col = "red")

# Buffer in dec degree: 0.1° = 11km
station_buffer <- st_buffer(station, dist = .1)
plot(st_geometry(station_buffer))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
donuts_in_station <- st_within(donuts, station_buffer)

station_with_donuts <- donuts_in_station %>%
  unlist(., use.names = FALSE) %>% unique
donuts_to_keep <- donuts_in_station %>%
  map_lgl(., function(x){ ifelse(length(x) > 0, TRUE, FALSE)})



plot(st_geometry(region_polygon))
plot(st_geometry(donuts[donuts_to_keep,]), add = TRUE, pch =
  20, col = "blue")
plot(st_geometry(station), add = TRUE, pch = 20, col = "green")
station_without_donuts <- slice(station, - station_with_donuts)
plot(st_geometry(station_without_donuts), add = TRUE, pch = 20, col = "red")

## There is 15 station without donuts: bigger buffers for them
station_buffer2 <- st_buffer(station_without_donuts, dist = .2)
plot(st_geometry(station_buffer2))
plot(st_geometry(donuts), add = TRUE, pch = 20, col = "green")
donuts_in_station2 <- st_within(donuts, station_buffer2)

station_with_donuts2 <- donuts_in_station2 %>%
  unlist(., use.names = FALSE) %>% unique
length(station_with_donuts2)
donuts_to_keep2 <- donuts_in_station2 %>%
  map_lgl(., function(x) { ifelse(length(x) > 0, TRUE, FALSE)})
which(donuts_to_keep2 == TRUE)

donuts_final <- donuts_to_keep | donuts_to_keep2

which(donuts_final == TRUE) %>% length

donuts_analysis  <- slice(donuts, which(donuts_final == TRUE))
mysave(donuts_analysis, dir = mypath("data"))

# Check
myload(donuts_analysis, dir = mypath("data"))
write_sf(donuts_analysis, mypath("data-raw", "flow_quality", "donuts_analysis.shp"))
#zip(mypath("data-raw", "flow_quality", "donuts_analysis"), files = )

###########################
#  Inspect flow metadata  #
###########################

stat_flow <- read_delim(mypath("data-raw", "flow_quality", "var_stat_FR_river_donuts.csv"), delim = ";")
var_flow <- read_delim(mypath("data-raw", "flow_quality", "var_donuts.csv"), delim = ";")
# We want by variables:
## the water column measurements
## the most events of measurements 
var_to_keep <- stat_flow %>%
  filter(com_code %in% "waterco") %>%
  group_by(var_code) %>%
  filter(nb_measures == max(nb_measures))
var_to_keep %<>%
  left_join(select(var_flow, var_code, fra_code, com_code, units))


###################
#  Get flow data  #
###################

library('dbplyr')
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "flow_fire", port = 5434)
DBI::dbListTables(con)

flow_data_db <- tbl(con, in_schema("public", "data_debit_danet"))
# Filter with donuts station in same river than station analysis: 
myload(station_analysis, donuts_analysis, dir = mypath("data"))

donuts <- donuts_analysis %>%
  filter(rht_name %in% station_analysis$rht_name) %>%
  mutate(id = as.character(id))

library("bit64")
# Get the good flow data
flow_data <- flow_data_db %>%
  mutate(id = as.character(id)) %>%
  collect()
length(which(unique(flow_data$code) %in% donuts$code))

# Get flow station that are in the same river than flow station:   
flow_data %<>%
  filter(id %in% donuts$id)

flow_data %<>%
  select(id, code, meas_date, value)

mysave(flow_data, dir = mypath("data-raw", "flow_quality"), overwrite = TRUE)

##################
#  Quality data  #
##################

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "water_quality_fire", port = 5434)
DBI::dbListTables(con)

quality_data <- tbl(con, in_schema("public", "data_qualite_danet"))

library('bit64')
quality_data %<>%
  mutate(id = as.character(id))
raw_filtered_water <- quality_data %>%
  group_by(var_code, fraction) %>%
  summarise(frac = n()) %>%
  collect() %>%
  mutate(frac = as.numeric(frac))
arrange(raw_filtered_water, var_code, fraction, desc(frac))
qplot(x = log10(frac), data = raw_filtered_water, geom = "histogram")

# To keep:
var_to_keep <- raw_filtered_water %>%
  filter(frac > 1000) %>%
  group_by(var_code) %>%
  arrange(desc(frac)) %>%
  slice(1) %>%
  ungroup()

quality_var <- quality_data %>%
  collect() %>%
  left_join(var_to_keep) %>%
  filter(!is.na(frac))

# Keep station that are in donuts
quality_var %<>%
  filter(id %in% donuts$id)

quality_data <- quality_var
mysave(quality_data, dir = mypath("data-raw"))
