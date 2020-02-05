
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
      betadiv_bin, beta_bin_c) %>%
    dplyr::left_join(sync, by = "station") %>%
    dplyr::rename(sync = synchrony,
      piel = pielou_med,
      prod = biomass_med,
      beta_bin = betadiv_bin
    )

  # Composition
  composition_axis <- nmds$points %>%
    as_tibble %>%
    dplyr::mutate(station = as.integer(dimnames(nmds$points)[[1]]))

  # Habitat -----------------------------------------------------------------
  habitat_sem <- hab_press %>% 
    dplyr::select(station, RC1, RC2, RC3, RC4, RC5)

  # Network -----------------------------------------------------------------
  net_sem <- network %>%
    dplyr::select(station, connectance_med, connectance_corrected_med, w_trph_lvl_avg_med) %>%
    dplyr::rename(
      ct = connectance_med,
      c_c = connectance_corrected_med,
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
      log_stab = log10(biomass_stab),
      log_bm = log10(prod)
    )
  return(dsem)

}

#' Compute the stability sem 
#'
#' @param .data data.frame
#'
compute_stab_sem_rich <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich, random = ~ 1 | basin, data = .data),
    #nlme::lme( ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich + piel + ct + t_lvl + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich + piel + ct + t_lvl
       + RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data)
  )
  sem <-  piecewiseSEM::as.psem(mod_list)
  if (get_sem) {
    return(sem)
  }
  output <- summary(sem, .progressBar = F)

  return(output)
}
compute_stab_sem <- function(.data, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::psem(
    nlme::lme(log_rich ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(c_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich + piel + c_c + t_lvl + beta_bin_c + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich + piel + c_c + t_lvl +
      beta_bin_c + RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data)
  )
  output <- summary(corsem, .progressBar = F)

  return(output)
}
#' compute generic sem
compute_gen_sem <- function(.data, list_mod, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::as.psem(list_mod)
  output <- summary(corsem, .progressBar = F)
  return(output)
}


#' Compute the productivity sem 
#'
#' @param .data data.frame
#'
compute_prod_sem <- function(.data, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::psem(
    nlme::lme(log_rich ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(c_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(prod ~ log_rich + piel + c_c + t_lvl + beta_bin_c + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data)
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
    nmds = NULL,
    basin = st_basin 
  )

  # Compute SEMs
  sem <- compute_stab_sem(.data = sem_data,
    random_effect = as.formula("~1|basin"))

  return(sem)

}

#' Compute PCA and rotated PCA over a list of dataset
#'
#' @param .data data.frame
#' @param naxis integer number of axis to keep. default to 2  
#'
compute_rotated_pca <- function(.data = NULL, naxis = 2) {
      .data %<>% na.omit
      pca <- ade4::dudi.pca(as.data.frame(.data), scannf = FALSE, nf = naxis, center = TRUE, scale = TRUE)
      pca_rotated <- psych::principal(.data, rotate="varimax", nfactors = naxis,
	scores=TRUE)

      return(list(normal = pca, rotated = pca_rotated))
  }


#' Compute plots from pca_rotated 
#'
#' @param pca_rotated list. The output of compute_rotated_pca.
#' @param axis integer vector of length 2 (x and y resp.).
#'
plot_rotated_pca <- function (pca_rotated = NULL, axis = c(1,2)) {

 {
   adegraphics::adegpar()$plabels
   ade4::s.corcircle(
    pca_rotated$rotated$loadings[1:nrow(pca_rotated$rotated$loadings),], xax = axis[1],
    yax = axis[2], box = TRUE)
  mtext(paste0("RC ", axis[1]), side = 1, adj = .5, line = -1)
  mtext(paste0("RC ", axis[2]), side = 2, adj = .5, line = -5)
  rotated_plot <- grDevices::recordPlot()
 }
  pca_plot <- factoextra::fviz_pca_var(pca_rotated$normal,
    axes = axis,
    col.var = "contrib", # Color by contributions to the PC
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Avoid text overlapping
  )
  return(list(rotated = rotated_plot, normal = pca_plot))
}

# Function to get bootstrap result on coefficient
mySumm2 <- function(.) {
  c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}
