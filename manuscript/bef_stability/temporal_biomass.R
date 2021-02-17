library(tidyverse)
library(magrittr)
library(ggpmisc)
library(cowplot)

mypath <- rprojroot::find_package_root_file
data_common <- mypath("data")
dest_dir <- mypath("data", "species")

source(mypath("R", "misc.R"))
source(mypath("R", "plot_methods.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "press_methods.R"))
source(mypath("R", "synchrony.R"))
source(mypath("R", "community_methods.R"))
source(mypath("R", "community_analysis.R"))
source(mypath("R", "statistical_analysis.R"))
source(mypath("R", "local_network_build.R"))
theme_set(theme_alain())


load(file = mypath("manuscript", "bef_stability", "result", "workspace.rda"))

myload(temporal_network_metrics, network_analysis, dir = mypath("data", "classes"))
temporal_network_metrics_classes <- temporal_network_metrics
myload(temporal_network_metrics, dir = dest_dir)
myload(community_metrics, temporal_community_metrics, synchrony, dir = data_common)
myload(op_analysis, trophic_level, trophic_class, dir = mypath("data"))


myload(
  hab_analysis,
  temporal_station_desc,
  temporal_press_polluants,
  geo_station,
  dir = mypath("data")
)
habitat_press <- build_habitat_pressure_dataset(
  .habitat_analysis = hab_analysis,
  .tmp_press = temporal_press_polluants,
  .tmp_st_desc = temporal_station_desc,
  .geo_st = geo_station 
)
# Make PCA 
habitat_press %<>% na.omit()
mask <- names(habitat_press) %in% names(get_pca_var_name_replacement())

pca_rotated <- compute_rotated_pca(
  .data = habitat_press[, mask],
  naxis = 2
)
habitat_pressure <- 
  cbind(habitat_press[, c("station", "temperature_med", "alt")], pca_rotated$rotated$score) %>%
  as_tibble

myload(op_analysis, dir = mypath("data"))
myload(biomass_ts_sax, dir = mypath("data"))
op_analysis_bbb <- filter(op_analysis, station %in%
  biomass_ts_sax[biomass_ts_sax$sax == "bbb",]$station)

com_data <- compute_community_temporal_analysis(.op = op_analysis_bbb,
 type_network_metrics = "classes"
)
# Compute sem dataset
st_basin <- get_basin_station(sf_obj = FALSE)
sem_data <- compute_sem_dataset(
  com = com_data[["tps_com"]],
  network = com_data[["tps_net"]],
  hab_press = habitat_pressure,
  sync = com_data[["sync_std"]],
  nmds = NULL,
  basin = st_basin
  )


community_metrics %<>%
  left_join(select(op_analysis_bbb, opcod, station, date, year))

sem_data %>%
  slice(
    which.min(abs(biomass_stab - quantile(sem_data$biomass_stab)[2])),
    which.min(abs(biomass_stab - quantile(sem_data$biomass_stab)[4])),
  )

sem_data %>%
  slice(
    which.max(piel)
  )

test <- community_metrics %>%
  filter(station == 1755)

test %<>%
  mutate(sp_tibble = map(sp_vector, ~tibble(species = names(.x), biomass_sp = .x))) %>%
  unnest(sp_tibble) %>%
  mutate(bm_std_sp = biomass_sp / surface) %>%
  select(station, date, species, bm_std_sp)

comb <- list(date = unique(test$date),
  species = unique(test$species)) %>% 
  expand.grid %>%
  as_tibble() %>%
  left_join(test, by = c("date", "species"))

comb[is.na(comb[["bm_std_sp"]]), ][["bm_std_sp"]] <- 0

comb %<>%
  group_by(species) %>%
  arrange(date) %>%
  mutate(mv_win = zoo::rollapply(data = bm_std_sp, width = 5, FUN = mean, na.rm = TRUE, fill = NA, partial = 2))

debugonce(plot_temporal_biomass)
plot_temporal_biomass(bm_data = comb,
  biomass_var = "mv_win",
  com = filter(sem_data, station == 1755),
  .log = FALSE)

# Get biomass by sp node:
net_sp_troph <- select(network_analysis, opcod, composition) %>%
  unnest(composition)
# Get synchrony and cv sp:
sync_sp_troph <- get_sync_cv_mat(com_analysis = net_sp_troph, op_analysis = op_analysis)
# Get trophic lvl by node
trophic_level %<>%
  rename(species = sp_class) %>%
  mutate(troph_group = get_size_class(trophic_level, NULL, troph_level,
      trophic_class))

plot_stab <- purrr::map(station_high_low_stab, function (x) {
  p <- plot_dyn_sp_biomass(sync = sync_sp_troph, trophic_level = trophic_level, station = x, .log = TRUE)
  p + theme(legend.position="none") 
})
plot_grid(plotlist = plot_stab, ncol = 2)



myload(community_metrics, op_analysis, dir = mypath("data"))


test <- community_metrics %>%
  left_join(select(op_analysis, station, opcod, year)) %>%
  filter(station == 1755)

test %<>%
  mutate(sp_tibble = map(sp_vector, ~tibble(species = names(.x), biomass_sp = .x))) %>%
  unnest(sp_tibble) %>%
  mutate(bm_std_sp = biomass_sp / surface) %>%
  select(station, year, species, bm_std_sp)

test %<>% 
  pivot_wider(names_from = species, values_from = "bm_std_sp")%>%
  mutate_at(vars(matches("[A-Z]{3}", ignore.case = FALSE)), ~ifelse(is.na(.), 0, .)) %>%
  pivot_longer(cols = matches("[A-Z]{3}", ignore.case = FALSE), names_to = "species", values_to = "bm_std_sp")

test %>%
  ggplot(aes(x = year, y = bm_std_sp, color = species)) +
  geom_line() +
  geom_line(data = test %>% group_by(year) %>% summarise(bm_std_sp = sum(bm_std_sp)) %>% mutate(species = "Total"))

test %>%
  ggplot(aes(x = year, y = bm_std_sp, color = species, fill = species)) +
  geom_area()
