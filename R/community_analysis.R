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

    if ("station" %in% names(com)) {
     com$station <- NULL
    }

  output <- com %>%
    dplyr::left_join(op, by = c("opcod")) %>%
    dplyr::filter(!(is.na(station) | is.na(opcod))) %>%
    dplyr::select(station, richness, nind, biomass, pielou, simpson) %>%
    dplyr::group_by(station) %>%
    dplyr::summarise_at(c("richness", "nind", "biomass", "pielou", "simpson"),
      list(avg = mean, cv =~ sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))
  return(output)
}

#' Compute temporal beta-diversity
#'
#' @param com data.frame community_analysis 
#'
#'
compute_temporal_betadiv <- function (.op = NULL, com = NULL) {

  com %<>%
    dplyr::ungroup() %>%
    dplyr::left_join(.op, by = "opcod") %>%
    dplyr::filter(!(is.na(station) | is.na(opcod))) %>%
    dplyr::select(station, date, species, nind) %>%
    dplyr::group_by(station, date, species) %>%
    dplyr::summarise(nind = sum(nind)) %>%
    dplyr::group_by(station) %>%
    dplyr::arrange(date) %>%
    tidyr::nest()
  ## build community matrices
  com %<>%
    dplyr::mutate(
      com = furrr::future_map2(data, station, function(x, y) {

	x %<>% tidyr::spread(species, nind) %>%
	  dplyr::mutate_if(is.integer, list(~replace(.,is.na(.), as.integer(0)))) %>%
	  dplyr::arrange(date) %>%
	  dplyr::select(-date)
	x
	}
      )
    )
  # Here I take the overall mean of differences between years but could be an idea
  # to consider the mean of dissimilarity year to year (i.e. the diagonal values)
  betadiv <- com %>%
    dplyr::mutate(
      betadiv = furrr::future_map_dbl(com, compute_betadiv),
      betadiv_bin = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE),
      betadiv_diag = furrr::future_map_dbl(com, compute_betadiv, time_to_time = TRUE),
      betadiv_bin_diag = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE, time_to_time = TRUE)
    )

} 

#' Compute synchrony with species
#'
#' 
#' @param com community_analysis
#'
#'
compute_com_synchrony <- function (.op = NULL, com = NULL) {

  synchrony <- get_sync_cv_mat(com_analysis = com,
    op_analysis = .op,
    presence_threshold = 0.5)

  synchrony %<>%
    dplyr::mutate(
      contrib = purrr::pmap(
	list(mat = com_mat, cv_com_tot = cv_com, cv_sp_tot = cv_sp, sync_tot = synchrony),
	~compute_sp_contrib(mat = ..1, cv_com_tot = ..2, cv_sp_tot = ..3, sync_tot = ..4))
    )

  synchrony %<>%
    dplyr::select(station, synchrony, cv_sp, cv_com, cv_classic, contrib)

  return(synchrony)
} 
#'
#'
#'
compute_community_temporal_analysis <- function(.op = NULL, dest_dir = NULL) {

  output <- vector("list", length = 4)
  names(output) <- c("tps_net", "tps_com", "tps_bm_troph", "sync")

  # Network ---------------------------------------------------------- 
  if(!exists("network_metrics")) {
    myload(network_metrics, dir = dest_dir)
  }
  output[["tps_net"]] <- summarise_network_over_time(op = .op, network = network_metrics)

  cat("Network done (1/4)\n")

  # Beta-diversity ---------------------------------------------------
  if(!exists("community_analysis")) {
    myload(community_analysis, dir = mypath("data"))
  }
  betadiv <- compute_temporal_betadiv(.op = .op, com = community_analysis)

  cat("Beta-diversity done (1.5/4)\n")

  # Synchrony of species ---------------------------------------------
  output[["sync"]] <- compute_com_synchrony(.op = .op, com = community_analysis)

  cat("Synchrony done (2/4)\n")

  # Biomass ---------------------------------------------------------- 
  if(!exists("community_metrics")) {
    myload(community_metrics, dir = mypath("data"))
  }
  output[["tps_com"]] <- summarise_com_over_time(
    op = .op,
    com = community_metrics
  )
  output[["tps_com"]] %<>%
    dplyr::left_join(dplyr::select(betadiv, -data, -com), by = "station")

  cat("Community description done (3/4)\n")

  # Biomass by trophic group -----------------------------------------
  if(!exists("network_analysis")) {
    myload(network_analysis, dir = dest_dir)
  }
  output[["tps_bm_troph"]] <- summarise_bm_troph_over_time(
    op = .op,
    network = network_analysis
  )

  cat("Biomass by trophic group done (4/4)\n")

  return(output)

}


#' Compute community matrix 
#'
#' 
#' @param com community_analysis
#'
#'
compute_com_mat <- function (.op = NULL, com = NULL) {

  com <- community_analysis %>%
    dplyr::select(opcod, species, biomass) %>%
    dplyr::left_join(op_analysis[, c("opcod", "station")], by = "opcod") %>%
    dplyr::filter(!is.na(station)) %>%
    dplyr::group_by(station) %>%
    tidyr::nest() %>%
    mutate(data = map(data, function (.data) {
	.data %<>%
	  tidyr::spread(species, biomass) %>%
	  dplyr::select(-opcod) %>%
	  dplyr::mutate_all(list(~ ifelse(is.na(.), 0, .))) %>%
	  dplyr::summarise_all(list(median))
	return(.data)
    }))
  
  com %<>%
    tidyr::unnest(data) %>%
    dplyr::mutate_all(list(~ ifelse(is.na(.), 0, .)))

  return(com)

} 
