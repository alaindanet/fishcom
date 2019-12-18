# Turn community analyze steps into functions 


#' Summarise network metrics over time
#'
#' Compute the median and the CV of network metrics through time 
#'
#' @param op data.frame containing opcod and station 
#' @param network data.frame containing opcod and network metrics 
#'
#'
summarise_network_over_time <- function (op = NULL, network = NULL,
  metrics = c("nestedness", "connectance", "connectance_corrected", "nbnode",
  "mean_troph_level", "mean_troph_level_corrected", "max_troph_level", "modularity", "modularity_corrected", "w_trph_lvl_avg")) {

  op %<>%
    dplyr::select(opcod, station, year)

  com <- op %>%
    dplyr::left_join(network_metrics, by = "opcod") %>%
    dplyr::group_by(station) %>%
    dplyr::rename(mean_troph_level = troph_level_avg,
      max_troph_level = troph_length) %>%
    dplyr::summarise_at(metrics,
      list(cv = ~sd(.) / mean(.), med = median))

  return(com)
}

#' Summarise Biomass and species richness by trophic group over time 
#'
#' This function computes biomass stability, species richness average and
#' median, etc, ... by trophic group (i.e. group 2 and 3).
#'
#' @inheritParams summarise_network_over_time
#'
summarise_bm_troph_over_time <- function (
  op = NULL,
  network = NULL
) {

  op %<>% dplyr::select(opcod, station, year)
  # Match network to fish operation 
  net <- network %>% 
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::ungroup() %>%
    filter(!is.na(station) & !is.na(opcod)) %>%
    dplyr::select(station, troph_group) %>%
    tidyr::unnest()

  # Summarise biomass and community characteristics
  biomass_variation <- net %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise_all(
      list(
	med = median,
	avg = mean,
	cv = ~sd(.) / mean(.),
	stab = ~mean(.) / sd(.))
    )

  # Select variables to keep:
  biomass_variation %<>%
    select_at(vars(dplyr::matches("biomass|richness_avg|richness_med|station|troph_group")))

  # Merge with temporal_network_metrics
  biomass_variation %<>%
    dplyr::group_by(station) %>%
    nest(.key = "troph_group")

  return(biomass_variation)

}

#' Summarise community metrics over time 
#'
#' @inheritParams summarise_network_over_time
#' @param com data.frame containing opcod and community description metrics 
#'
summarise_com_over_time <- function (
  op = NULL,
  com = NULL
) {
  com %<>%
    dplyr::left_join(op, by = c("opcod")) %>%
    dplyr::filter(!(is.na(station) | is.na(opcod))) %>%
    dplyr::select(station, richness, nind, biomass, pielou, simpson) %>%
    dplyr::group_by(station) %>%
    dplyr::summarise_at(c("richness", "nind", "biomass", "pielou", "simpson"),
      list(avg = mean, cv =~ sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))
  return(com)
}
