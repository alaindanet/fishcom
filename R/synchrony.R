################################################################################
#                        Functions to compute synchrony                        #
################################################################################
compute_avg_cv_sp <- function(biomass, variance) {
  #Check that the species are in the same order in the vector:
  stopifnot(names(biomass) == names(variance))

  rel_biomass <- biomass / sum(biomass)
  rel_sdt <- sqrt(variance) / biomass

  cv_avg <- sum(rel_biomass * rel_sdt)
  return(cv_avg)
}

compute_cv_com <- function(synchrony, cv_sp) {
  sqrt(synchrony) * cv_sp
}

compute_synchrony <- function(cov_mat) {
  com_var <- sum(cov_mat)
  var_intra_sp <- sum(sqrt(diag(cov_mat)))
  phi <- com_var / (var_intra_sp^2)
  return(phi)
}

get_sync_cv_mat <- function(com_analysis = NULL, op_analysis = NULL, presence_threshold = 0.5) {

  # If it is network data, change the "species" colum
  stopifnot(any(c("species", "sp_class") %in% names(com_analysis)))
  if ("sp_class" %in% names(com_analysis)) {
    com_analysis %<>% dplyr::rename(species = sp_class)
    troph <- TRUE
  } else {
    troph <- FALSE 
  }

  com <- dplyr::ungroup(com_analysis) %>%
    dplyr::left_join(dplyr::select(op_analysis, opcod, station, date), by = "opcod") %>%
    dplyr::filter(!is.na(station))

  if (troph) {
    com %<>% dplyr::select(station, date, species, biomass, troph_level,troph_group)
  } else {
    com %<>% dplyr::select(station, date, species, biomass)
  }
  
  # We will compute the variance of each species in each station, which
  # require that there is some obsversation
  filter_na <- com %>%
    dplyr::group_by(station, species) %>%
    dplyr::summarise(n = sum(!is.na(biomass)) / dplyr::n()) %>%
    dplyr::mutate(to_rm = ifelse(n < presence_threshold, TRUE, FALSE)) %>%
    dplyr::select(-n)

  com %<>%
    dplyr::left_join(filter_na) %>%
    dplyr::filter(!to_rm) %>%
    dplyr::select(-to_rm)

  # Get 0 biomass when the species is absent of station
  # By station replicate complete species list observed:
  complete_com <- com %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(
      species = list(unique(species)),
      date = list(unique(date))) %>%
    dplyr::mutate(comb = purrr::map2(species, date, function(sp, date){
	test <- expand.grid(species = sp, date = date)
	return(test)
})
      ) %>%
    tidyr::unnest(comb)

  # Join and put biomass to 0 when no observed:
  complete_com %<>% dplyr::left_join(com, by = c("station", "species", "date")) %>%
    dplyr::mutate(biomass = ifelse(is.na(biomass), 0, biomass))

  if (troph) {
    troph_data <- complete_com[, c("station", "species", "date", "biomass", "troph_level", "troph_group")] %>%
      dplyr::group_by(station) %>%
      tidyr::nest()
    complete_com %<>%
      dplyr::select(-troph_level, -troph_group)
  }
  complete_com %<>%
    dplyr::group_by(station) %>%
    tidyr::nest()

  complete_com %<>%
    dplyr::mutate(
      com_mat_date = purrr::map(data, function(x) tidyr::spread(x, species, biomass)),
      com_mat = purrr::map(com_mat_date, function(x) dplyr::select(x, -date)),
      com_mat = purrr::map(com_mat, as.matrix)
    )

  synchrony <- complete_com %>%
    dplyr::mutate(
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

  # To get biomass ans troph information:
  if (troph) {
    synchrony$data <- troph_data$data
  }

  return(synchrony)

}
