
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
  sync %<>% dplyr::select(station, synchrony, cv_sp, richness_tot)
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
      log_rich_tot = log10(richness_tot),
      log_sync = log10(sync), 
      log_cv_sp = log10(cv_sp),
      log_stab = log10(biomass_stab),
      log_bm = log10(prod),
      log_RC1 = log10(RC1 + abs(min(RC1)) + 1),
      log_RC2 = log10(RC2 + abs(min(RC2)) + 1),
      log_RC3 = log10(RC3 + abs(min(RC3)) + 1)
    )
  return(dsem)

}

#' Compute the stability sem 
#'
#' @param .data data.frame
#'
compute_stab_sem_rich <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich_tot ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    #nlme::lme( ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich_tot + ct + t_lvl + log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich_tot + ct + t_lvl
       + log_RC1 + log_RC2 + log_RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data)
  )
  sem <-  piecewiseSEM::as.psem(mod_list)
  if (get_sem) {
    return(sem)
  }
  output <- summary(sem, .progressBar = F)

  return(output)
}
compute_stab_sem_rich_beta <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich_tot ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich_tot  + ct + t_lvl + RC1 + RC2 + RC3 + RC4 + RC5 + beta_bin,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich_tot + ct + t_lvl
       + RC1 + RC2 + RC3 + RC4 + RC5 + beta_bin, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data)
    #, piel %~~% t_lvl
  )
  sem <-  piecewiseSEM::as.psem(mod_list)
  #sem <- mod_list
  if (get_sem) {
    return(sem)
  }
  output <- summary(sem, .progressBar = F)

  return(output)
}
compute_stab_sem_meta <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich_tot ~ RC1 + RC2,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    #nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    #nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    #nlme::lme(beta_bin ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich_tot  + ct + t_lvl  + piel + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich_tot + ct + t_lvl
       + piel + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    lm(log_stab ~ log_cv_sp + log_sync, data = .data),
    piel %~~% t_lvl,
    piel %~~% log_rich_tot, 
    #beta_bin %~~% log_rich_tot, 
    t_lvl %~~% log_rich_tot, 
    ct %~~% log_rich_tot 

  )
  sem <-  piecewiseSEM::as.psem(mod_list)
  #sem <- mod_list
  if (get_sem) {
    return(sem)
  }
  output <- summary(sem, .progressBar = F)

  return(output)
}
compute_stab_sem <- function(.data, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::psem(
    nlme::lme(log_rich_tot ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(c_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich_tot + piel + c_c + t_lvl + beta_bin_c + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich_tot + piel + c_c + t_lvl +
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
    nlme::lme(log_rich_tot ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(log_bm ~ log_rich_tot + piel + ct + t_lvl + beta_bin_c + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data)
  )
  output <- summary(corsem, .progressBar = F)

  return(output)
}
compute_prod_sem_rich <- function(.data, random_effect = "~ 1 | basin") {
  corsem <- piecewiseSEM::psem(
    nlme::lme(log_rich_tot ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    #nlme::lme(beta_bin_c ~ RC1 + RC2 + RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(log_bm ~ log_rich_tot + ct + t_lvl + RC1 + RC2 + RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data)
  )
  output <- summary(corsem, .progressBar = F)

  return(output)
}
compute_prod_sem_rich_beta <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich_tot ~ RC1 + RC2 + RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    nlme::lme(piel ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(beta_bin ~ RC1 + RC2 + RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(prod ~ log_rich_tot + piel + ct + t_lvl + RC1 + RC2 + RC3 + RC4 + RC5 + beta_bin,
      random = ~ 1 | basin, data = .data)
  )
  sem <-  piecewiseSEM::as.psem(mod_list)
  if (get_sem) {
    return(sem)
  }
  output <- summary(sem, .progressBar = F)

  return(output)
}

#' Compute indirect effect
#'
#' @param sem
#' @param type character "stab" or "bm" 
#' 
compute_stab_sem_indirect <- function (sem = NULL, type = "stab", p_val_thl = NULL) {

  params <- sem$coefficients
  colnames(params)[length(colnames(params))] <- "stars"

  # Preliminary coeff col modif
  params %<>%
    dplyr::rename(
      resp = Response,
      pred = Predictor,
      p_val = P.Value,
      std_est = Std.Estimate
    ) %>%
    dplyr::select(resp, pred, p_val, std_est)

  if (!is.null(p_val_thl)) {
    params %<>% dplyr::filter(p_val < p_val_thl)
  }

  # A. filter effects
  evt_var <- c(paste0("log_RC", seq(1,3)), paste0("RC", seq(4,5)))
  names(evt_var) <- evt_var
  stab_comp_var <- c("log_sync", "log_cv_sp")
  names(stab_comp_var) <- stab_comp_var 
  rich_var <- c("log_rich_tot")
  names(rich_var) <- rich_var 
  com_var <- c("ct", "t_lvl")
  names(com_var) <- com_var

  #1. effect on stab
  #commute all the indirect on stab through cv_sp and sync 

  ## Get CV and sync effect on stab
  sync_cv_sp_on_stab <- purrr::map_dbl(stab_comp_var, function (x, fit) {
    fit[fit$resp == "log_stab" & fit$pred == x, ]$std_est
    }, fit = params)

  ## Get indirect com effect on stab +
  #temp indirect effect of rich, evt on stab through direct effect on cv_sp and
  #sync

  variable <- c(evt_var, com_var, rich_var)
  indir_var_stab <- purrr::map_dbl(variable, function (x, fit, stab_comp_est) {

    indir_sync <- fit[fit$resp == "log_sync" & fit$pred == x, ]$std_est * stab_comp_est["log_sync"]
    indir_cv_sp <- fit[fit$resp == "log_cv_sp" & fit$pred == x, ]$std_est * stab_comp_est["log_cv_sp"]

    if (any(length(indir_sync), length(indir_cv_sp)) == 0) {
      indir_sync <- ifelse(length(indir_sync) == 0, 0, indir_sync)
      indir_cv_sp <- ifelse(length(indir_cv_sp) == 0, 0, indir_cv_sp)
    }
      
    output <- indir_sync + indir_cv_sp
    return(output)
    }, fit = params, stab_comp_est = sync_cv_sp_on_stab)


  #B. compute indirect effects

  #1. Richness
  ## Effect of richness on com
  rich_com_effect <- purrr::map_dbl(com_var, function (x, fit, rich_var) {
    params[params$resp == x & params$pred == rich_var, ]$std_est
    }, fit = params, rich_var = rich_var)

  rich_com_effect <- purrr::map_dbl(rich_com_effect, ~ifelse(length(.x) == 0, 0, .x))

  ## Total effect of richness on stability  
  indir_rich <- 
     rich_com_effect["ct"] * indir_var_stab["ct"] +
     rich_com_effect["t_lvl"] * indir_var_stab["t_lvl"] +
     indir_var_stab[rich_var]
  names(indir_rich) <- rich_var

   #2. Environment
   indir_evt <- purrr::map_dbl(evt_var, function (x, fit, indir_rich, indir_var_stab) {
     tmp <- vector(mode = "list", length = 4)

     # effect through richness:
     tmp[[1]] <-
       params[params$resp == rich_var & params$pred == x, ]$std_est *
       indir_rich
     # effect through connectance
     tmp[[2]] <-
       params[params$resp == "ct" & params$pred == x, ]$std_est *
       indir_var_stab["ct"]
     # effect through trophic level
     tmp[[3]] <-
       params[params$resp == "t_lvl" & params$pred == x, ]$std_est *
       indir_var_stab["t_lvl"]
     # effect through sync and cv_sp
     tmp[[4]] <- indir_var_stab[x]

     tmp <- map_dbl(tmp, ~ifelse(length(.x) == 0, 0, .x))

     return(sum(tmp))
    
    }, fit = params, indir_rich = indir_rich, indir_var_stab = indir_var_stab)

   #3. Community
   indir_com <- indir_var_stab[names(indir_var_stab) %in% com_var]

   #C. output
   output <- c(indir_evt, indir_rich, indir_com)
   return(output)
}


#' Build dataset, compute sem and gather result 
#'
#' @inheritParams compute_community_temporal_analysis
#'
build_dataset_get_sem_coefficient <- function (.op = NULL, sem_fun = compute_stab_sem_rich_beta, dest_dir = NULL) {

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
  sem <- sem_fun(.data = sem_data,
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
