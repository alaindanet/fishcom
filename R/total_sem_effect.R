#' Get stab sem total effect

#' Get direct effect on response sem
#' @param fit 
#' @param predictor chr vector 
#' @param response chr string
get_direct_effect_sem <- function (fit = NULL, predictor = NULL, response = NULL, p_val_thl = NULL,
  output_names = c("predictor", "response")
  ) {
  # Check argument
  if (all(length(predictor) >1, length(response) > 1)) {
   stop("only one of predictor and response should have length superior to one") 
  }
  # Filtering
  mask <- fit$resp %in% response & fit$pred %in% predictor 
  res <- fit[mask, ]
  ## check mask
  stopifnot(any(mask == TRUE))
  if (!is.null(p_val_thl)) {
    res[res$p_val > p_val_thl, "std_est"] <- 0
  }
  # Prepare output
  output <- res$std_est
  stopifnot(output_names %in% c("predictor", "response"))
  if (output_names == "predictor") {names(output) <- res$pred} else {names(output) <- res$resp}
  return(output)
}
#' Get indirect effect
get_indirect_effect <- function (fit = NULL, p_val_thl = NULL, ...) {
  level <- list(...)

  extraction <- function (x = NULL, y = NULL, fit = NULL, p_val_thl = NULL) {
    output <- expand.grid(pred = x, resp = y)
    output %<>%
      mutate_at(vars(c("pred", "resp")), as.character)
    output %<>%
    mutate(std_est = purrr::map2_dbl(
	resp, pred,
	~get_fit(response = .x, predictor = .y,
	  p_val_thl = p_val_thl, fit = fit)))
    return(output)
  }
  get_fit <- function(response, predictor, p_val_thl, fit) {
    res <- fit[fit$resp == response & fit$pred == predictor, ]
    if (!is.null(p_val_thl)) {
      res[res$p_val > p_val_thl, "std_est"] <- 0
    }
    return(res$std_est)
  }

  compute_relation <- function (x = NULL, y = NULL, ...) {
    
    left_var <- letters[y]
    right_var <- letters[x]
    est_var <- paste0("est_", letters[x], letters[y])

    extraction(x = level[[x]], y = level[[y]],
      fit = fit, p_val_thl = p_val_thl) %>%
    mutate(
      !! left_var := resp,
      !! right_var := pred,
      !! est_var := std_est) %>%
    filter(resp %in% fit$resp & pred %in% fit$pred) %>%
    select(!!! c(left_var, right_var, est_var))
  }

  index <- seq(1, length(level))
  relation_list <- map2(index[-length(index)], index[2:length(index)],
    ~compute_relation(x = .x, y = .y)
  )

  tmp <- relation_list[[1]]
  tmp$indir <- tmp[, colnames(tmp) == "est_ab"]
  if (length(index) > 2) {
    
  for(i in 2:(length(index) - 1)) {
    tmp <- left_join(tmp, relation_list[[i]], by = letters[i])
  }

  for(i in 2:(length(index) - 1)) {
    col_to_multiply <- paste0("est_", letters[i], letters[i + 1])
    tmp$indir <- tmp$indir * tmp[, colnames(tmp) == col_to_multiply] 
  }

  }

  return(tmp[, order(names(tmp))])
}

#' Get env effect on stab 
get_env_on_stab <- function (fit = NULL, p_val_thl = NULL) {

  via_cv_sp_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab") 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab") 
  )
  # effect of env through richness through com 
  via_richness_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab")
  )

  # effect of env through com 
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    z = c("ct", "t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab")
  )
  to_sum <- list(via_cv_sp_sync = via_cv_sp_sync, via_richness = via_richness,
    via_com = via_com, via_richness_com = via_richness_com)

  from_indir_to_tot(to_sum = to_sum)
}

#' Get env effect on sync cv_sp 
get_env_on_stab_comp <- function (fit = NULL, p_val_thl = NULL, stab_comp = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    zz = stab_comp
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    zz = stab_comp
  )
  # effect of env through com 
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zz = stab_comp
  )
  to_sum <- list(direct = direct, via_richness = via_richness, via_com = via_com)
  from_indir_to_tot(to_sum = to_sum)
}

#' get env on com 
get_env_on_com <- function (fit = NULL, p_val_thl = NULL, com = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    zz = com 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    zz = com 
  )
  to_sum <- list(direct = direct, via_richness = via_richness)

  from_indir_to_tot(to_sum = to_sum)
}
#' Get env on richness
get_env_on_rich <- function (fit = NULL, p_val_thl = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    zz = "log_rich_tot" 
  )
  to_sum <- list(direct = direct)

  from_indir_to_tot(to_sum = to_sum)
}

#' Get richness effect on stab 
get_rich_on_stab <- function (fit = NULL, p_val_thl = NULL) {

  via_cv_sp_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = "log_rich_tot",
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab") 
  )
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c("log_stab") 
  )
  to_sum <- list(via_cv_sp_sync = via_cv_sp_sync, via_com = via_com)

  from_indir_to_tot(to_sum = to_sum)
}

#' Get richness effect on stab 
get_rich_on_stab_comp <- function (fit = NULL,
  p_val_thl = NULL, stab_comp = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = "log_rich_tot",
    zz = stab_comp 
  )
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zz = stab_comp 
  )
  to_sum <- list(direct = direct, via_com = via_com)

  from_indir_to_tot(to_sum = to_sum)
}

#' Get env effect on bm 
get_env_on_bm <- function (fit = NULL, p_val_thl = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    zzz = c("log_bm") 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    zzz = c("log_bm") 
  )

  # effect via richness and com 
  via_richness_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zzz = c("log_bm") 
  )
  # effect of env through com 
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
    z = c("ct", "t_lvl"),
    zzz = c("log_bm") 
  )
  to_sum <- list(direct = direct, via_richness = via_richness,
    via_richness_com = via_richness_com, via_com = via_com)

  from_indir_to_tot(to_sum = to_sum)
}

#' Get richness effect on bm 
get_rich_on_bm <- function (fit = NULL, p_val_thl = NULL) {

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = "log_rich_tot",
    zzz = c("log_bm") 
  )
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c("log_rich_tot"),
    z = c("ct", "t_lvl"),
    zzz = c("log_bm") 
  )
  to_sum <- list(direct = direct, via_com = via_com)

  from_indir_to_tot(to_sum = to_sum)
}

#' From indirect effect to total effect
from_indir_to_tot <- function (to_sum = NULL) {
  indir_via <- map(to_sum,
    ~ .x %>%
      group_by(a) %>%
      summarise(indir = sum(indir))
  )
  # Total effect
  indir_tot <- do.call(rbind, indir_via)
  tot <- indir_tot %>% 
    group_by(a) %>%
    summarise(total = sum(indir))
  return(round(tibble::deframe(tot), 2))
}

#' Convert piecewisesem object
#' @param  piecewisesem object
get_coefficient_piecewisesem <- function (sem = NULL, p_val_thl = NULL) {

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
    params %<>% dplyr::filter(p_val <= p_val_thl)
  }

  return(params)

}

#' Compute total effects stab sem 
#'
#' @param sem
#' @param type character "stab" or "bm"
#' 

compute_stab_sem_indirect <- function (sem = NULL, type = "stab", p_val_thl = NULL) {


  # A. filter effects
  params <- get_coefficient_piecewisesem(sem = sem, p_val_thl = NULL)

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
  sync_cv_sp_on_stab <- get_direct_effect_sem(
    fit = params,
    response = "log_stab",
    predictor = stab_comp_var,
    p_val_thl = p_val_thl,
    output_names = "predictor"
    )
  ## Ct and tlvl
  ### Effect on sync on log_cv_sp
  com_effect_on_stab_comp <- purrr::map(com_var, 
    ~get_direct_effect_sem(
    fit = params,
    predictor = .x,
    response = stab_comp_var,
    p_val_thl = 0.05,
    output_names = "response"
    )
  )
  ### Effect on stab through sync and cv_sp
  #
  purrr::map(com_effect_on_stab_comp,
    function(x, sync_cv_sp) {
      out <- x 
      # indirect effect through stab_comp:
      to_add <- sum() 
    }, sync_cv_sp = sync_cv_sp_on_stab)
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
