
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
  sync %<>% dplyr::select(station, synchrony, cv_sp, richness_tot, rich_tot_std)
  biomass_sem <- com %>%
    dplyr::select(
      station,
      biomass_stab, bm_std_stab, 
      biomass_med, bm_std_med,
      richness_med, rich_std_med,
      pielou_med,
      betadiv_bin, beta_bin_c) %>%
    dplyr::left_join(sync, by = "station") %>%
    dplyr::rename(
      sync = synchrony,
      piel = pielou_med,
      prod = biomass_med,
      prod_std = bm_std_med,
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
    dplyr::select(station, connectance_med, w_trph_lvl_avg_med) %>%
    dplyr::rename(
      ct = connectance_med,
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
      log_rich_tot_std = log10(rich_tot_std),
      log_sync = log10(sync), 
      log_cv_sp = log10(cv_sp),
      log_stab = log10(biomass_stab),
      log_stab_std = log10(bm_std_stab),
      log_bm = log10(prod),
      log_bm_std = log10(prod_std),
      log_RC1 = log10(RC1 + abs(min(RC1)) + 1),
      log_RC2 = log10(RC2 + abs(min(RC2)) + 1),
      log_RC3 = log10((RC3)*(-1) + abs(min((RC3)*(-1))) + 1)
    )
  return(dsem)

}

#' Compute the stability sem 
#'
#' @param .data data.frame
#'
compute_stab_sem_rich <- function(.data, random_effect = "~ 1 | basin", get_sem = FALSE) {

  mod_list <- list(
    nlme::lme(log_rich_tot_std ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot_std, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot_std, random = ~ 1 | basin, data = .data),
    #nlme::lme( ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot, random = ~ 1 | basin, data = .data),
    nlme::lme(log_sync ~ log_rich_tot_std + ct + t_lvl + log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,
      random = ~ 1 | basin, data = .data),
    nlme::lme(log_cv_sp ~ log_rich_tot_std + ct + t_lvl
       + log_RC1 + log_RC2 + log_RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    lm(log_stab_std ~ log_cv_sp + log_sync, data = .data)
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
    nlme::lme(log_rich_tot_std ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,  random = ~ 1 | basin, data = .data),
    #nlme::lme(piel ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(ct ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot_std, random = ~ 1 | basin, data = .data),
    nlme::lme(t_lvl ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5 + log_rich_tot_std, random = ~ 1 | basin, data = .data),
    #nlme::lme(beta_bin_c ~ log_RC1 + log_RC2 + log_RC3 + RC4 + RC5, random = ~ 1 | basin, data = .data),
    nlme::lme(log_bm_std ~ log_rich_tot_std + ct + t_lvl + log_RC1 + log_RC2 + log_RC3 + RC4 + RC5,
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

#' Compute total effects stab sem 
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

  # define variables 
  evt_var <- c(paste0("log_RC", seq(1,3)), paste0("RC", seq(4,5)))
  names(evt_var) <- evt_var
  stab_comp_var <- c("log_sync", "log_cv_sp")
  names(stab_comp_var) <- stab_comp_var 
  stab_var <- "log_stab"
  names(stab_var) <- stab_var 
  rich_var <- c("log_rich_tot")
  names(rich_var) <- rich_var 
  com_var <- c("ct", "t_lvl")
  names(com_var) <- com_var

  # Prepare output 
  output_names <- c(com_var, rich_var, evt_var)
  output <- vector(mode = "list", length = length(output_names))
  names(output) <- output_names 

  ## Get CV and sync effect on stab
  sync_cv_sp_on_stab <- purrr::map_dbl(stab_comp_var, function (x, fit) {
    fit[fit$resp == "log_stab" & fit$pred == x, ]$std_est
    }, fit = params)

  ## Ct and tlvl
  effect_of_com <- purrr::map(com_var,
    function (x, fit, stab_comp_est) {

    output_names <- c(stab_comp_var, stab_var) 
    output <- vector(mode = "list", length = length(output_names))
    names(output) <- output_names 

    # Compute direct effect of com_var on sync and 
    output[names(output) %in% stab_comp_var] <- purrr::map(stab_comp_var,
      function (stab_part, fit) {
	direct_effect <- fit[fit$resp == stab_part & fit$pred == x, ]$std_est
	direct_effect <- ifelse(length(direct_effect) == 0, 0, direct_effect)
	names(direct_effect) <- "direct"
	return(direct_effect)
      }, fit = params)

    # Compute indirect effect through synchrony and cv_sp: 
    output[[stab_var]] <- purrr::map_dbl(stab_comp_var,
      function (stab_part, sync_cv_effect) {
	indir_effect <- output[[stab_part]][1] * sync_cv_effect[stab_part]
	indir_effect <- ifelse(length(indir_effect) == 0, 0, indir_effect)
	return(indir_effect)
      }, sync_cv_effect = sync_cv_sp_on_stab)

    return(output)

    }, fit = params)

  output[names(output) %in% com_var] <- effect_of_com


  ## Species richness
  effect_of_rich <- vector(mode = "list", length = length(c(stab_comp_var, stab_var)))
  names(effect_of_rich) <- c(stab_comp_var, stab_var) 
  ### Effect on sync and cv_sp
  effect_of_rich[names(effect_of_rich) %in% stab_comp_var] <- purrr::map(stab_comp_var,
    function(stab_part, fit, effect_of_com) {
      # Direct effect
      direct_effect <- fit[fit$resp == stab_part & fit$pred == rich_var, ]$std_est
      direct_effect <- ifelse(length(direct_effect) == 0, 0, direct_effect)
      names(direct_effect) <- "direct"
      # via com: 
      via_com_effect <- purrr::map_dbl(com_var, function (com, fit, effect_of_com) {

      effect <- fit[fit$resp == com & fit$pred == rich_var, ]$std_est * #effect of richness on com 
	effect_of_com[[com]][[stab_part]]["direct"] # effect of t_lvl on stab_part (sync or cv_sp)
      effect <- ifelse(length(effect) == 0, 0, effect)
      return(effect)
      }, fit = params, effect_of_com = effect_of_com)
      # Return
      out <- c(direct_effect, via_com_effect)
      return(out)
    }, fit = params, effect_of_com = effect_of_com)
  ### Effect on stab
  effect_of_rich[[stab_var]] <- purrr::map_dbl(stab_comp_var,
    function(stab_part, fit, sync_cv_effect) {
    
      # effect of richness directly + via ct + via tlvl  
      effect <- sum(map_dbl(effect_of_rich[[stab_part]],
	  function (x) {
	    res <- x * sync_cv_effect[stab_part]
	    res <- ifelse(length(res) == 0, 0, res)
	  })
      )
      return(effect)
      }, sync_cv_effect = sync_cv_sp_on_stab, fit = params)

  output[[rich_var]] <- effect_of_rich

  ## Environment 
  env_on_com <- purrr::map(evt_var,
    function (evt, fit) {

    output_names <- c(com_var, stab_comp_var, stab_var) 
    output <- vector(mode = "list", length = length(output_names))
    names(output) <- output_names

    ### Effect of environment on com
    output[names(output) %in% com_var] <- purrr::map(com_var,
	function (com, fit, evt) {
	  direct_effect <- fit[fit$resp ==  com & fit$pred == evt, ]$std_est
	  direct_effect <- ifelse(length(direct_effect) == 0, 0, direct_effect)
      
	  via_richness <- fit[fit$resp == rich_var & fit$pred == evt, ]$std_est * #evt on richness
	    fit[fit$resp == com & fit$pred == rich_var, ]$std_est #richness on com 
	  via_richness <- ifelse(length(via_richness) == 0, 0, via_richness)

	  out <- c(direct_effect, via_richness) 
	  names(out) <- c("direct", rich_var)
	  return(out)
	}, fit = params, evt = evt)

    ### Effect of environment on stab component 
    output[names(output) %in% stab_comp_var] <- purrr::map(stab_comp_var,
	function (stab_part, fit, evt, effect_of_com_on_stab_part) {
	  direct_effect <- fit[fit$resp ==  stab_part & fit$pred == evt, ]$std_est
	  direct_effect <- ifelse(length(direct_effect) == 0, 0, direct_effect)
      
	  via_com <- purrr::map_dbl(com_var,
	    function(com) {
	      res <- fit[fit$resp == com & fit$pred == evt, ]$std_est *
		effect_of_com_on_stab_part[[com]][[stab_part]]
	      res <- ifelse(length(res) == 0, 0, res)
	      return(res)
	    }) 

	  # TODO: add paths through com

	  # effect of richness directly + via ct + via tlvl  
	  via_richness <- sum(map_dbl(effect_of_rich[[stab_part]],
	  function (x, sync_cv_effect) {
	    res <- fit[fit$resp == rich_var & fit$pred == evt, ]$std_est * #effect of evt on rich
	      x * # effect of rich on stab part 
	      sync_cv_effect[stab_part] #effect of stab part on stab 
	    res <- ifelse(length(res) == 0, 0, res)
	  }, sync_cv_effect = sync_cv_sp_on_stab))
	  out <- c(direct_effect, via_com, via_richness) 
	  names(out) <- c("direct", com_var, rich_var)
	  return(out)
      
	}, fit = params, evt = evt, effect_of_com_on_stab_part = effect_of_com)

    ### Effect of environment on stability 
    output[[stab_var]] <- purrr::map_dbl(stab_comp_var,
      function (stab_part, sync_cv_effect) {
	indir_effect <- sum(output[[stab_part]]) * sync_cv_effect[stab_part]
	indir_effect <- ifelse(length(indir_effect) == 0, 0, indir_effect)
	return(indir_effect)
      }, sync_cv_effect = sync_cv_sp_on_stab)

    return(output)

    }, fit = params)

  output[names(output) %in% evt_var] <- env_on_com

  return(output)


  #### Effect on stab comp
  #effect_on_stab_comp <- purrr::map(stab_comp_var,
    #function (stab_var, fit, effect_on_com, evt) {

      ###### direct
      #direct_effect <- fit[fit$resp == stab_var & fit$pred == evt, ]$std_est
      #direct_effect <- ifelse(length(direct_effect) == 0, 0, direct_effect)
      ###### com
      #effect_via_com <- purrr::map_dbl(com_var,
	#function (com) {
	  #out <- effect_on_com[com]["direct"] * effect_of_com[com][stab_var]
	  #out <- ifelse(length(out) == 0, 0, out)
	#})
      ###### richness
      #effect_via_richness <- fit[fit$resp == rich_var & fit$pred == evt, ]$std_est *
	#effect_of_rich[stab_var]
      #effect_via_richness <- ifelse(length(effect_via_richness) == 0, 0, effect_via_richness)

      #out <- c(direct_effect, effect_via_com, effect_via_richness)
      #names(out) <- c("direct", com_var, rich_var) 
      #return(out)
      

      ###### Effect on stab
      #evt_on_stab <- purrr::map_dbl(stab_comp_var,
	#function (stab_var, fit, effect_on_stab_comp) {
	
	  #out <- sum(effect_on_stab_comp[stab_var]["direct"], effect_on_stab_comp[stab_var]["richness"])*
	    #fit[fit$resp == stab_var & fit$pred == evt, ]$std_est

	  #out <- ifelse(length(out) == 0, 0, out)
	  #return(out)
	
	#}, fit = params, effect_on_stab_comp = effect_on_stab_comp)  

      #out <- c(effect_on_com, effect_on_stab_comp, evt_on_stab)
      #return(out)
    #}, fit = params)

  ### Effect of evironment on stab
  #output_evt_stab <- c(stab_comp_var, stab_var)
  #evt_on_stab <- vector(mode = "list", length = length(output_evt_stab))
  #names(evt_on_stab) <- output_evt_var

  #output[output_evt_var] <- effect_on_com

  #return(output)
    #indir_sync <- fit[fit$resp == "log_sync" & fit$pred == x, ]$std_est * stab_comp_est["log_sync"]
    #indir_cv_sp <- fit[fit$resp == "log_cv_sp" & fit$pred == x, ]$std_est * stab_comp_est["log_cv_sp"]

    #if (any(c(length(indir_sync), length(indir_cv_sp)) == 0)) {
      #indir_sync <- ifelse(length(indir_sync) == 0, 0, indir_sync)
      #indir_cv_sp <- ifelse(length(indir_cv_sp) == 0, 0, indir_cv_sp)
    #}
    #output <- c(indir_sync, indir_cv_sp, indir_sync + indir_cv_sp)
    #names(output) <- c("sync", "cv_sp", "stab")
    #return(output)
    #}, fit = params, stab_comp_est = sync_cv_sp_on_stab)


  ### Get indirect com effect on stab +
  ##temp indirect effect of rich, evt on stab through direct effect on cv_sp and
  ##sync
  #variable <- c(evt_var, com_var, rich_var)
  #indir_var_stab <- purrr::map(variable, function (x, fit, stab_comp_est) {

    #indir_sync <- fit[fit$resp == "log_sync" & fit$pred == x, ]$std_est * stab_comp_est["log_sync"]
    #indir_cv_sp <- fit[fit$resp == "log_cv_sp" & fit$pred == x, ]$std_est * stab_comp_est["log_cv_sp"]

    #if (any(c(length(indir_sync), length(indir_cv_sp)) == 0)) {
      #indir_sync <- ifelse(length(indir_sync) == 0, 0, indir_sync)
      #indir_cv_sp <- ifelse(length(indir_cv_sp) == 0, 0, indir_cv_sp)
    #}
    #output <- c(indir_sync, indir_cv_sp, indir_sync + indir_cv_sp)
    #names(output) <- c("sync", "cv_sp", "stab")
    #return(output)
    #}, fit = params, stab_comp_est = sync_cv_sp_on_stab)


  ##B. compute indirect effects

  #output <- vector(mode = "list", length = 4)
  #names(output) <- c("richness", "community", "environment", "sync_cv_sp") 
  #output$community <- indir_var_stab[com_var]

  ##1. Richness
  ### Effect of richness on com
  #rich_com_effect <- purrr::map(com_var, function (x, fit, rich_var) {
    #fit[fit$resp == x & fit$pred == rich_var, ]$std_est
    #}, fit = params, rich_var = rich_var)

  #rich_com_effect <- purrr::map_dbl(rich_com_effect, ~ifelse(length(.x) == 0, 0, .x))

  ### Total effect of richness on stability
  #indir_rich <- 
     #rich_com_effect["ct"] * indir_var_stab["ct"] +
     #rich_com_effect["t_lvl"] * indir_var_stab["t_lvl"] +
     #indir_var_stab[rich_var]
  #names(indir_rich) <- rich_var
  #output$richness <- c(
    #indir_rich,
    #rich_com_effect["ct"] * indir_var_stab["ct"],
    #rich_com_effect["t_lvl"] * indir_var_stab["t_lvl"],
    #indir_var_stab[rich_var]
  #)
  #names(output$richness) <- c("total", "ct", "t_lvl", "rich")

   ##2. Environment
   #indir_evt <- purrr::map(evt_var, function (x, fit, indir_rich, indir_var_stab) {
     #tmp <- vector(mode = "list", length = 4)

     ## effect through richness:
     #tmp[[1]] <-
       #fit[fit$resp == rich_var & fit$pred == x, ]$std_est *
       #indir_rich
     ## effect through connectance
     #tmp[[2]] <-
       #fit[fit$resp == "ct" & fit$pred == x, ]$std_est *
       #indir_var_stab["ct"]
     ## effect through trophic level
     #tmp[[3]] <-
       #fit[fit$resp == "t_lvl" & fit$pred == x, ]$std_est *
       #indir_var_stab["t_lvl"]
     ## effect through sync and cv_sp
     #tmp[[4]] <- indir_var_stab[x]

     #tmp <- map_dbl(tmp, ~ifelse(length(.x) == 0, 0, .x))
     #tmp <- c(tmp, sum(tmp))
     #names(tmp) <- c("richness", "ct", "t_lvl", "sync_cv_sp", "total")

     #return(tmp)
    
    #}, fit = params, indir_rich = indir_rich, indir_var_stab = indir_var_stab)
  #output$environment <- indir_evt

  #C. output
}

#' Compute total effects bm sem 
#'
#' @param sem
#' 
compute_bm_sem_indirect <- function (sem = NULL, p_val_thl = NULL) {

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
  bm_var <- c("log_bm")
  names(bm_var) <- bm_var 
  rich_var <- c("log_rich_tot")
  names(rich_var) <- rich_var 
  com_var <- c("ct", "t_lvl")
  names(com_var) <- com_var

  #B. compute indirect effects
  output <- vector(mode = "list", length = 2)
  names(output) <- c("richness", "environment") 
  #output$community <- indir_var_stab[com_var]

  #1. Richness

  ## Effect of com on bm
  com_bm_effect <- purrr::map_dbl(com_var, function (x, fit, rich_var, bm_var) {
    out <- fit[fit$resp == bm_var & fit$pred == x, ]$std_est
    if (length(out) == 0) {0} else {out}
    }, fit = params, rich_var = rich_var, bm_var = bm_var)

  ## Effect of richness on com
  rich_com_effect <- purrr::map_dbl(com_var, function (x, fit, rich_var) {
    out <- fit[fit$resp == x & fit$pred == rich_var, ]$std_est
    if (length(out) == 0) {0} else {out}
    }, fit = params, rich_var = rich_var)

  rich_com_effect <- purrr::map_dbl(rich_com_effect, ~ifelse(length(.x) == 0, 0, .x))

  ## Total effect of richness on stability  
  indir_rich <- 
     rich_com_effect["ct"] * com_bm_effect["ct"] +
     rich_com_effect["t_lvl"] * com_bm_effect["t_lvl"] +
     params[params$resp == bm_var & params$pred == rich_var, ]$std_est
  names(indir_rich) <- rich_var
  output$richness <- c(indir_rich,
    rich_com_effect["ct"] * com_bm_effect["ct"],
    rich_com_effect["t_lvl"] * com_bm_effect["t_lvl"],
    params[params$resp == bm_var & params$pred == rich_var, ]$std_est
  )
  names(output$richness) <- c("total", "ct", "t_lvl", "rich")

  #2. Environment

  extract_evt_effect <- function (x, fit, indir_rich, com_bm_effect) {
    tmp <- vector(mode = "list", length = 4)

    # effect through richness:
    tmp[[1]] <-
      fit[fit$resp == rich_var & fit$pred == x, ]$std_est *
      indir_rich
    # effect through connectance
    tmp[[2]] <-
      fit[fit$resp == "ct" & fit$pred == x, ]$std_est *
      com_bm_effect["ct"]
    # effect through trophic level
    tmp[[3]] <-
      fit[fit$resp == "t_lvl" & fit$pred == x, ]$std_est *
      com_bm_effect["t_lvl"]
    # direct effect on bm
    tmp[[4]] <- fit[fit$resp == "log_bm" & fit$pred == x, ]$std_est 

    tmp <- map_dbl(tmp, ~ifelse(length(.x) == 0, 0, .x))
    tmp <- c(tmp, sum(tmp))
    names(tmp) <- c("richness", "ct", "t_lvl", "bm", "total")

    return(tmp)

  }

  #debug(extract_evt_effect)
  indir_evt <- purrr::map(evt_var, extract_evt_effect, fit = params, indir_rich = indir_rich, com_bm_effect = com_bm_effect)
  output$environment <- indir_evt

   #3. Community
   indir_com <- com_bm_effect 

   #C. output
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

#' Model troph group and richness
#'
#' @param .data data.frame with richness, basin and troph_group 
#' @param resp string response variable of the model 
compute_stat_troph_rich <- function (.data = NULL, resp = NULL) {

  output <- vector(mode = "list", length = 3) 
  names(output) <- c("all", "rsq", "predict")

  output[["all"]] <- .data %>%
    dplyr::ungroup() %>%
    tidyr::nest(data = -troph_group)

  # Formula: 
  myformula <- as.formula(paste0(resp, " ~ log_rich_tot_std + (1 | basin)"))

  # Model and CI:
  output[["all"]] %<>%
    dplyr::mutate(
      model = purrr::map(data,
	~lmer(data = na.omit(.x),
	  formula = myformula
	  )),
      coefs = purrr::map(model, ~fixef(.x)),
      rsq   = purrr::map(model, ~piecewiseSEM::rsquared(.x, method = "nagelkerke") %>%
	mutate_at(vars(Conditional, Marginal), ~round(., 2))),
      boot  = purrr::map(model, ~bootMer(.x, mySumm2, nsim = 100)),
      ci    = purrr::map(boot, ~confint(.x))
    )

  # Prediction of the model:

  output[["all"]] %<>%
    dplyr::mutate(
      pred = purrr::map(model, ~ggpredict(.x, terms = c("log_rich_tot_std"), type = "fe")),
      pred_df = purrr::map(pred,
	~.x %>%
	  as_tibble() %>%
	  rename(!!resp := predicted, log_rich_tot_std = x)
	),
	plot = purrr::map(data,
	  ~ggplot(.x, aes(y = sym(resp), x = log_rich_tot_std)) +
	    geom_point() +
	    geom_smooth(method = "lm")
	)
    )

  # Format Rsq and prediction: 
  output[["rsq"]] <- output[["all"]] %>%
    select(troph_group, rsq) %>%
    unnest(rsq)

  output[["predict"]] <- output[["all"]] %>%
    select(troph_group, pred_df) %>%
    unnest(pred_df)

  return(output)

}

#' Compute PCA and rotated PCA over a list of dataset @param .data data.frame @param naxis integer number of axis to keep. default to 2  
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

print_coef <- function (vec){
  round(abs(vec)*100)
}

print_ci <- function(.df){
  .df <- round(.df, 3)*100
  
  paste0("[", .df[["2.5 %"]],";",.df[["97.5 %"]] ,"]")
}

get_clean_sem_coef <- function(sem = NULL, resp = NULL, pred = NULL) {
  out <- sem[sem$Response == resp & sem$Predictor == pred, ]$Std.Estimate
  out <- ifelse(length(out) == 0, 0, out)
  round(out, 2)
}

permutation_test <- function (
  x = NULL,
  y = NULL,
  nb_perm = NULL
) {

  obs_diff <- mean(x) - mean(y)

  null_dist <- map_dbl(1:nb_perm, function(id) {

    x_sim <- sample(c(x, y), size = length(x), replace = FALSE)
    y_sim <- sample(c(x, y), size = length(y), replace = FALSE)

    null_diff <- mean(x_sim) - mean(y_sim) 
    return(null_diff)
})

    output <- vector(mode = "list", length = 4)
    names(output) <- c("obs", "null", "null_dist", "p_val") 

    output$obs <- obs_diff
    output$null <- mean(null_dist)
    output$null_dist <- null_dist
    output$p_val <- (length(which(abs(null_dist) > abs(obs_diff))) + 1) / nb_perm

    return(output)
}

test_troph <- function (
  .data = NULL,
  var = NULL,
  nb_perm = 2000,
  na_omit = TRUE 
) {

  low_troph <- .data[.data$troph_group == "2",][[var]] 
  high_troph <- .data[.data$troph_group == "3",][[var]] 

  if (na_omit) {
    low_troph <- na.omit(low_troph)
    high_troph <- na.omit(high_troph)
  }

 permutation_test(
  x = high_troph,
  y = low_troph,
  nb_perm = nb_perm 
 )
} 

