# Turn community analyze steps into functions 

#' Remove species from community and network
#' 
#' @param 
#' @param com community_analysis
rm_sp_from_com <- function (com = NULL, sp_to_rm = NULL) {

  com %<>%
    filter(!species %in% sp_to_rm)

  return(com)

}

#' Summarise network metrics over time
#'
#' Compute the median and the CV of network metrics through time 
#'
#' @param op data.frame containing opcod and station 
#' @param network data.frame containing opcod and network metrics 
#'
#'
summarise_network_over_time <- function (op = NULL, class_network = NULL,
  species_network = NULL,
  metrics = c("nestedness", "connectance", "connectance_corrected", "nbnode",
  "mean_troph_level", "mean_troph_level_corrected", "max_troph_level", "modularity", "modularity_corrected", "w_trph_lvl_avg")) {

  op %<>%
    dplyr::select(opcod, station, year)

  # Metrics are computed with species network
  com <- op %>%
    dplyr::left_join(species_network, by = "opcod") %>%
    dplyr::group_by(station) %>%
    #dplyr::rename(mean_troph_level = troph_level_avg,
      #max_troph_level = troph_length) %>%
    dplyr::summarise_at(metrics,
      list(cv = ~sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))

    ##  Trophic group metrics are computed with class network
    # Get total richness, biomass_med,  by station and trophic group 
  composition <- class_network %>%
    dplyr::left_join(op, by = "opcod") %>%
    tidyr::unnest(composition) %>%
    mutate(species = str_extract_all(sp_class, "[A-Z]{3}"))

  richness_tot <- composition %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise(richness_tot = length(unique(species)))

  ## Biomass
  troph_group <- class_network %>%
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::select(station, opcod, troph_group) %>%
    unnest(troph_group) %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise_at(c("biomass", "richness", "nbnode"),
      list(cv = ~sd(.) / mean(.), med = median, avg = mean, stab = ~mean(.) / sd(.)))

  # Merge trophic group metrics
  troph_group_metrics <- troph_group %>%
    dplyr::left_join(richness_tot, by = c("station", "troph_group")) %>%
    dplyr::group_by(station) %>%
    tidyr::nest(.key = "troph_group")

  com %<>% dplyr::left_join(troph_group_metrics, by = "station")

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
    dplyr::select(station, date, species, biomass) %>%
    dplyr::group_by(station, date, species) %>%
    dplyr::summarise(biomass = sum(biomass)) %>%
    dplyr::group_by(station) %>%
    dplyr::arrange(date) %>%
    tidyr::nest()
  ## build community matrices
  com %<>%
    dplyr::mutate(
      com = furrr::future_map2(data, station, function(x, y) {

	x %<>%
	  tidyr::spread(species, biomass) %>%
	  dplyr::mutate_at(vars(-date), list(~replace(.,is.na(.), as.integer(0)))) %>%
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

#' Compute betadiv
#'
#'
#'@export
compute_betadiv <- function(com, binary = FALSE, time_to_time = FALSE) {
  if (binary) {
    com %<>% replace(., . != 0, 1)
  }

  dist_mat <- vegan::vegdist(com, method = "bray", binary = binary)

  if (time_to_time) {
    dist_mat <- diag(dist_mat)
  }

  mean(dist_mat)
}

#' Compute synchrony with species
#'
#' 
#' @param com community_analysis
#'
#'
compute_com_synchrony <- function (.op = NULL, com = NULL, ...) {

  synchrony <- get_sync_cv_mat(com_analysis = com,
    op_analysis = .op, presence_threshold = NA)

  synchrony %<>%
    dplyr::mutate(
      contrib = purrr::pmap(
	list(mat = com_mat, cv_com_tot = cv_com, cv_sp_tot = cv_sp, sync_tot = synchrony),
	~compute_sp_contrib(mat = ..1, cv_com_tot = ..2, cv_sp_tot = ..3, sync_tot = ..4)),
      richness_tot = map_dbl(com_mat, ~ncol(.x))
    )

  synchrony %<>%
    dplyr::select(station, richness_med, synchrony, cv_sp, cv_com, cv_classic, contrib, richness_tot)

  return(synchrony)
} 
#'
#'
#'
compute_community_temporal_analysis <- function(.op = NULL, dest_dir = NULL) {

  output <- vector("list", length = 4)
  names(output) <- c("tps_net", "tps_com", "tps_bm_troph", "sync")

  # Network ---------------------------------------------------------- 
  myload(network_metrics, dir = mypath("data", "classes"))
  class_network_metrics <- network_metrics
  myload(network_metrics, dir = mypath("data", "species"))
  species_network_metrics <- network_metrics
  rm(network_metrics)

  output[["tps_net"]] <- summarise_network_over_time(op = .op,
    species_network = species_network_metrics,
    class_network = class_network_metrics,
    metrics = c("connectance", "w_trph_lvl_avg"))

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
  myload(network_analysis, dir = mypath("data", "classes"))
  class_network_analysis <- network_analysis
  rm(network_analysis)

  output[["tps_bm_troph"]] <- summarise_bm_troph_over_time(
    op = .op,
    network = class_network_analysis
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

  com %<>%
    dplyr::select(opcod, species, biomass) %>%
    dplyr::left_join(op_analysis[, c("opcod", "station")], by = "opcod") %>%
    dplyr::filter(!is.na(station)) %>%
    dplyr::group_by(station) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, function (.data) {
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

#' Compute Pielou and simpson
#'
#' @param .data a vector of species abundance/biomass 
#'
compute_pielou_simpson <- function(.data) {
  richness <- vegan::specnumber(.data)
  if (richness == 1) {
    pielou <- 0
    simpson <- 0
  } else {
    pielou <- vegan::diversity(.data) / log(richness)
    simpson <- vegan::diversity(.data, index = "simpson")
  }
  out <- tibble( pielou = pielou, simpson = simpson)
  return(out)
}

#' Compute synchrony over basin
#'
#'
#'
compute_synchrony_over_basin <- function(com = NULL, basin = NULL, .op = NULL) {

  com %<>%
    dplyr::left_join(dplyr::select(.op, opcod, station, year), by = "opcod") %>%
    dplyr::left_join(basin, by = "station")

  # Get mean biomass by basin, species and by year
  complete_com <- com %>%
    dplyr::group_by(basin) %>%
    dplyr::summarise(
      species = list(unique(species)),
      year = list(unique(year))) %>%
    dplyr::mutate(comb = purrr::map2(species, year, function(sp, date){
        test <- expand.grid(species = sp, year = date)
        return(test)
    })
      ) %>%
    tidyr::unnest(comb)

  # Join and put biomass to 0 when no observed:
  complete_com %<>% dplyr::left_join(com, by = c("basin", "species", "year")) %>%
    dplyr::mutate(biomass = ifelse(is.na(biomass), 0, biomass)) %>%
    group_by(basin, species, year) %>%
    summarise(biomass = mean(biomass))

  complete_com %<>%
    group_by(basin) %>%
    nest()

  complete_com %<>%
    dplyr::mutate(
      com_mat_date = purrr::map(data, function(x) tidyr::spread(x, species, biomass)),
      com_mat = purrr::map(com_mat_date, function(x) dplyr::select(x, -year)),
      com_mat = purrr::map(com_mat, as.matrix)
    )

  synchrony <- complete_com %>%
    dplyr::mutate(
      richness = purrr::map(com_mat, compute_sp_nb_from_com_mat),
      richness_med = purrr::map_dbl(richness, median),
      richness_tot = map_dbl(com_mat, ~ncol(.x)),
      avg_sp = purrr::map(com_mat, colMeans),
      cov_mat = purrr::map(com_mat, cov),
      var_sp = purrr::map(cov_mat, diag),
      synchrony = purrr::map_dbl(cov_mat, compute_synchrony),
      cv_sp = purrr::map2_dbl(avg_sp, var_sp, compute_avg_cv_sp),
      cv_com = compute_cv_com(synchrony = synchrony, cv_sp = cv_sp),
      cv_classic = purrr::map2_dbl(cov_mat, com_mat, function(variance, biomass) {
        sqrt(sum(variance)) / mean(rowSums(biomass))
})
    )
  synchrony

}
