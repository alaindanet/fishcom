################################################################################
#                    Prepare data for flow and quality data                    #
################################################################################

library('tidyverse')
library('magrittr')
library('sf')
library('rgeos')
#devtools::load_all()
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "flow_quality")
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

myload(stations_FR_river_donuts)
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

mysave(flow_data, dir = mypath("data-raw"), overwrite = TRUE)

#########################
#  Get flow by station  #
#########################
# Problem is that flow station are not the same than quality station and match
# very little with fishing station. May be that we have to interpolate them
# with openSTARS.

myload(station_analysis, donuts_analysis, dir = mypath("data"))

# Match the station and donuts which are on the same drain_id 
station_id <- station_analysis %>%
  as_tibble() %>%
  rename(station = id) %>%
  mutate(
    drain_id = as.character(drain_id),
    trhyd_id = as.character(trhyd_id)
    ) %>%
  select(station, drain_id, trhyd_id)
donuts_id <- donuts_analysis %>%
  as_tibble() %>%
  select(id, drain_id, trhyd_id, rht_name) %>%
  mutate(
    drain_id = as.character(drain_id),
    id = as.character(id),
    trhyd_id = as.character(trhyd_id)
    )

mask_trhyd_donuts_in_station <- which(donuts_id$trhyd_id %in% station_analysis$trhyd_id)
which(donuts_id[mask_trhyd_donuts_in_station, ]$trhyd_id %in%
flow_data$trhyd_id) %>%
length()
length(which(donuts_id$trhyd_id %in% flow_data$trhyd_id))

donuts_station <- donuts_id %>%
  left_join(select(station_id, station, trhyd_id), by = "trhyd_id") %>%
  filter(!is.na(station))

myload(flow_data, dir = mypath("data-raw"))

flow_data %<>%
  mutate(id = as.character(id)) %>%
  left_join(donuts_station) %>%
  filter(!is.na(station))
length(unique(donuts_id$drain_id))
length(unique(flow_data$drain_id))
flow_stat <- flow_data %>%
  group_by(station) %>%
  summarise(nobs = n(), date_min = min(meas_date), date_max = max(meas_date))
qplot(x = nobs, data = flow_stat, geom = "histogram")
arrange(flow_stat, desc(nobs))

# Suppress aberrant values
flow_data %<>%
  mutate(value = replace(value, value < 0, 0))

# Put station:  


# Select on date
flow_day <- flow_data %>%
  group_by(trhyd_id, meas_date) %>%
  summarise(avg = mean(value))
filter(flow_day, is.na(avg))

#  

##################
#  Quality data  #
##################

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "water_quality_fire", port = 5434)
DBI::dbListTables(con)

quality_data <- tbl(con, in_schema("public", "data_qualite_danet"))

donuts_station <- donuts_id %>%
  left_join(select(station_id, station, trhyd_id), by = "trhyd_id") %>%
  filter(!is.na(station))
quality_data %<>%
  filter(id %in% donuts_station$id) %>%
  collect()
library('bit64')
quality_data %<>%
  mutate(id = as.character(id)) %>%
  left_join(select(donuts_station, id, station), by = "id")

quality_stat <- quality_data %>%
  group_by(station, var_code) %>%
  summarise(nobs = n(), date_min = min(meas_date), date_max = max(meas_date))
qplot(x = nobs, data = quality_stat, geom = "histogram")

# Timing avg between measurements: 
timing_bw_meas <- quality_data %>%
  group_by(station, var_code) %>%
  mutate(
    point = seq(1, length(var_code)),
    sample_sep = c(NA, meas_date[-1] - meas_date[-length(var_code)])
  ) %>%
  summarise(avg_sep = mean(sample_sep, na.rm = TRUE))
qplot(x = avg_sep, data = timing_bw_meas, geom = "histogram")


year_quality <- quality_data %>%
  mutate(year = lubridate::year(meas_date)) %>%
  group_by(station, year, var_code) %>%
  summarise(avg = mean(value))

# Select years corresponding to op extent by station:  
test <- year_quality %>%
  group_by(station) %>%
  summarise(date_min = min(year), date_max = max(year)) %>%
  list(
    qplot(x = date_min, data = ., geom = "histogram", main = "starting date"),
    qplot(x = date_max, data = ., geom = "histogram", main = "ending date")
  )
cowplot::plot_grid(test[[2]], test[[3]])
## get the op:
myload(op_analysis, dir = mypath("data"))
### Get the min and max date:
op_analysis %>%
  group_by(station) %>%
  summarise(start_date = min(date), end_date = max(date))

filter(op_analysis, station == 262)
#strange

# look for pulse:
## by station, found extrem values and get their number 
quantile_quality <- quality_data %>%
  mutate(year = lubridate::year(meas_date)) %>%
  group_by(station, var_code) %>%
  summarise(q10 = quantile(value, probs = .1), q90 = quantile(value, probs = .9))

## join pulse to station and compute the number of pulse:
pulse_data <- quality_data %>%
  left_join(quantile_quality, by = c("var_code", "station")) %>%
  group_by(station, var_code) %>%
  summarise(q10 = sum(value <= q10), q90 = sum(value >= q90)) %>%
  gather(pulse_type, nb, q10, q90)

pulse_data %>%
  filter(var_code == "BOD")
