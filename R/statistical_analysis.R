
#' Compute statistical analysis
#'
#'
#'
#'
#'
compute_sem_dataset <- function (
  com = NULL,
  network = NULL, 
  hab_press = NULL,
  sync = NULL,
  nmds = NULL,
  basin = NULL
  ) {
  # Check:
  #stopifnot(
    #any(
      ##map_lgl(list("com", "network", "hab_press", "sync", "nmds"), ~missing(get(.x)))
      #missing(nmds),
      #missing(com),
      #missing(network),
      #missing(hab_press),
      #missing(sync)
      #)
  #)

  # Community -----------------------------------------------------------------

  # Detrend turnover from richness
  mod <- lm(betadiv_bin ~ log(richness_med), data = com)
  com$beta_bin_c <- resid(mod)

  # Add synchrony
  sync %<>% dplyr::select(station, synchrony, cv_sp)
  biomass_sem <- com %>%
    dplyr::select(station, biomass_stab, biomass_med, richness_med, pielou_med,
      beta_bin_c) %>%
    dplyr::left_join(sync, by = "station") %>%
    dplyr::rename(sync = synchrony,
      piel = pielou_med,
      prod = biomass_med 
    )

  # Composition
  composition_axis <- nmds$points %>%
    as_tibble %>%
    mutate(station = as.integer(dimnames(nmds$points)[[1]]))

  # Habitat -----------------------------------------------------------------
  habitat_sem <- hab_press %>% 
    select(station, lat, alt, width_river_mean, DBO_med, DBO_cv, flow_med, flow_cv,
      temperature_med, temperature_cv, med_evt1, med_evt2, cv_evt1, cv_evt2, med_press1, med_press2, cv_press1, cv_press2) %>%
  rename(width = width_river_mean)

  # Network -----------------------------------------------------------------
  net_sem <- network %>%
    dplyr::select(station, connectance_corrected_med, w_trph_lvl_avg_med) %>%
    dplyr::rename(c_c = connectance_corrected_med,
      t_lvl = w_trph_lvl_avg_med)

  # Merge and return --------------------------------------------------------
  dsem <- biomass_sem  %>%
    dplyr::left_join(net_sem, by = "station") %>%
    dplyr::left_join(habitat_sem, by = "station") %>%
    dplyr::left_join(basin, by = "station") %>%
    dplyr::left_join(composition_axis, by = "station") %>%
    na.omit()

  dsem %<>%
    dplyr::mutate(
      log_rich = log10(richness_med),
      log_sync = log10(sync), 
      log_cv_sp = log10(cv_sp),
      log_stab = log10(biomass_stab)
    )
  return(dsem)

}

#' Compute the stability sem 
#'
#' @param .data data.frame
#'
compute_stab_sem <- function(.data, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::psem(
    nlme::lme(log_rich ~ med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 +
      med_press2 + cv_press1 + cv_press2,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 +
      med_press2 + cv_press1 + cv_press2, random = ~ 1 | basin, data = .data),
    nlme::lme(c_c ~ med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 + med_press2 + cv_press1 + cv_press2, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 + med_press2 + cv_press1 + cv_press2, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin_c ~ med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 + med_press2 + cv_press1 + cv_press2, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich + piel + c_c + t_lvl + beta_bin_c + med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 + med_press2 + cv_press1 + cv_press2,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich + piel + c_c + t_lvl +
      beta_bin_c + med_evt1 + med_evt2 + cv_evt1 + cv_evt2 + med_press1 + med_press2 + cv_press1 + cv_press2, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data)
  )
  output <- summary(corsem, .progressBar = F)

  return(output)
}

#' Build dataset, compute sem and gather result 
#'
#' @inheritParams compute_community_temporal_analysis
#'
build_dataset_get_sem_coefficient <- function (.op = NULL, dest_dir = NULL) {

  # Compute stability, network metrics, etc with the new datasets
  com_data <- compute_community_temporal_analysis(.op = .op, dest_dir = dest_dir)

  # Compute sem dataset 
  sem_data <- compute_sem_dataset(
    com = com_data[["tps_com"]],
    network = com_data[["tps_net"]], 
    hab_press = habitat_pressure, 
    sync = com_data[["sync"]],
    nmds = nmds,
    basin = st_basin 
  )

  # Compute SEMs
  sem <- compute_stab_sem(.data = sem_data,
    random_effect = as.formula("~1|basin"))

  return(sem)

}
