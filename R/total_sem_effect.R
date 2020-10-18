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
 tmp <- tmp[, order(names(tmp))]
  return(tmp[, c("a", "indir")])
}

#' Get env effect on stab 
get_env_on_stab <- function (fit = NULL, p_val_thl = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  stab_var <- "log_stab"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
   stab_var <- paste0(stab_var, "_", type)
  }

  via_cv_sp <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zz = c("log_cv_sp"),
    zzz = c(stab_var) 
  )
  
  
  via_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zz = c("log_sync"),
    zzz = c(stab_var) 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var) 
  )
  # effect of env through richness through com 
  via_richness_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    z = c("ct"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var)
  )

  via_richness_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    z = c("t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var)
  )

  # effect of env through com 
  via_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    z = c("ct"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var)
  )

  via_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    z = c("t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var)
  )

  to_sum <- list(
    via_cv_sp = via_cv_sp,
    via_sync = via_sync,
    via_richness = rbind(via_richness, via_richness_ct, via_richness_tlvl),
    via_ct = via_ct,
    via_tlvl = via_tlvl
  )

  return(get_indir_common_output(.data = to_sum))
}

#' Get env effect on sync cv_sp 
get_env_on_stab_comp <- function (fit = NULL, p_val_thl = NULL,
  stab_comp = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  if (type == "std") {
    rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zz = stab_comp
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    zz = stab_comp
  )
  # effect of env through com 
  via_richness_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    z = c("ct", "t_lvl"),
    zz = stab_comp
  )

  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    z = c("ct", "t_lvl"),
    zz = stab_comp
  )
  to_sum <- list(
    direct = direct,
    via_richness = via_richness,
    via_rich_com = via_richness_com,
    via_com = via_com
  )

  return(get_indir_common_output(.data = to_sum))
}

#' get env on com 
get_env_on_com <- function (fit = NULL, p_val_thl = NULL, com = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zz = com 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    zz = com 
  )
  to_sum <- list(direct = direct, via_richness = via_richness)

  return(get_indir_common_output(.data = to_sum))
}
#' Get env on richness
get_env_on_rich <- function (fit = NULL, p_val_thl = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zz = rich_var 
  )
  to_sum <- list(direct = direct)

  return(get_indir_common_output(.data = to_sum))
}

#' Get richness effect on stab 
get_rich_on_stab <- function (fit = NULL, p_val_thl = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  stab_var <- "log_stab"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
   stab_var <- paste0(stab_var, "_", type)
  }


  via_cv_sp <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = rich_var,
    zz = c("log_cv_sp"),
    zzz = c(stab_var) 
  )
  
  via_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = rich_var,
    zz = c("log_sync"),
    zzz = c(stab_var) 
  )

  via_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c(rich_var),
    z = c("ct"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var) 
  )
  via_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c(rich_var),
    z = c("t_lvl"),
    zz = c("log_sync", "log_cv_sp"),
    zzz = c(stab_var) 
  )

  to_sum <- list(
    via_cv_sp = via_cv_sp,
    via_sync = via_sync,
    via_ct = via_ct,
    via_tlvl = via_tlvl
  )

  return(get_indir_common_output(.data = to_sum))
}

#' Get richness effect on stab 
get_rich_on_stab_comp <- function (fit = NULL,
  p_val_thl = NULL, stab_comp = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = rich_var,
    zz = stab_comp 
  )
  via_com <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = rich_var,
    z = c("ct", "t_lvl"),
    zz = stab_comp 
  )
  to_sum <- list(direct = direct, via_com = via_com)

  return(get_indir_common_output(.data = to_sum))
}

#' Get com on stab
get_ct_on_stab <- function (fit = NULL,
  p_val_thl = NULL, stab_comp = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  stab_var <- "log_stab"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
   stab_var <- paste0(stab_var, "_", type)
  }

  via_cv_sp <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c("ct"),
    zz = c("log_cv_sp"),
    zzz = c(stab_var) 
  )
  
  via_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c("ct"),
    zz = c("log_sync"),
    zzz = c(stab_var) 
  )

  to_sum <- list(
    via_cv_sp = via_cv_sp,
    via_sync = via_sync
  )

  return(get_indir_common_output(.data = to_sum))
}

get_tlvl_on_stab <- function (fit = NULL,
  p_val_thl = NULL, stab_comp = NULL, type = "std") {

  rich_var <- "log_rich_tot"
  stab_var <- "log_stab"
  if (type == "std") {
   rich_var <- paste0(rich_var, "_", type)
   stab_var <- paste0(stab_var, "_", type)
  }

  via_cv_sp <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c("t_lvl"),
    zz = c("log_cv_sp"),
    zzz = c(stab_var) 
  )
  
  via_sync <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = c("t_lvl"),
    zz = c("log_sync"),
    zzz = c(stab_var) 
  )

  to_sum <- list(
    via_cv_sp = via_cv_sp,
    via_sync = via_sync
  )

  return(get_indir_common_output(.data = to_sum))
}


#' Get env effect on bm 
get_env_on_bm <- function (fit = NULL, p_val_thl = NULL, type = "std") {

  bm_var <- "log_bm"
  rich_var <- "log_rich_tot"
  if (type == "std") {
   bm_var <- paste0(bm_var, "_", type)
   rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    zzz = c(bm_var) 
  )
  # effect of env through richness 
  via_richness <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    zzz = c(bm_var) 
  )

  # effect via richness and com 
  via_richness_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    z = c("ct"),
    zzz = c(bm_var) 
  )

  via_richness_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    y = c(rich_var),
    z = c("t_lvl"),
    zzz = c(bm_var) 
  )
  # effect of env through com 
  via_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    z = c("t_lvl"),
    zzz = c(bm_var) 
  )
  via_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = sem_env_var(),
    z = c("ct"),
    zzz = c(bm_var) 
  )

  to_sum <- list(direct = direct,
    via_richness = rbind(via_richness, via_richness_ct, via_richness_tlvl),
    via_ct = via_ct, via_tlvl = via_tlvl)

  return(get_indir_common_output(.data = to_sum))
}

#' Get richness effect on bm 
get_rich_on_bm <- function (fit = NULL, p_val_thl = NULL, type = "std") {

  bm_var <- "log_bm"
  rich_var <- "log_rich_tot"
  if (type == "std") {
   bm_var <- paste0(bm_var, "_", type)
   rich_var <- paste0(rich_var, "_", type)
  }

  direct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    x = rich_var,
    zzz = c(bm_var) 
  )
  via_ct <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c(rich_var),
    z = c("ct"),
    zzz = c(bm_var)
  )
  via_tlvl <- get_indirect_effect(fit = fit, p_val_thl = 0.05,
    y = c(rich_var),
    z = c("t_lvl"),
    zzz = c(bm_var)
  )
  to_sum <- list(direct = direct, via_ct = via_ct, via_tlvl = via_tlvl)


  return(get_indir_common_output(.data = to_sum))
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
#' Get indirect effect 
get_indir <- function (to_sum = NULL) {
  indir_via <- map(to_sum,
    function (x) {
      x %<>%
	group_by(a) %>%
	summarise(indir = sum(indir))

    return(round(tibble::deframe(x), 2))
    }
  )
  # Total effect
  indir_via
}

get_indir_common_output <- function (.data = NULL) {
  c(
    get_indir(to_sum = .data),
    list(total = from_indir_to_tot(to_sum = .data))
  )

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

sem_env_var <- function() {
  #c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5)))
  paste0("log_RC", seq(1,2))
}

get_clean_sem_coef <- function(sem = NULL, resp = NULL, pred = NULL) {
  out <- sem[sem$Response == resp & sem$Predictor == pred, ]$Std.Estimate
  out <- ifelse(length(out) == 0, 0, out)
  round(out, 2)
}
