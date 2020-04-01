
#' Compute average trophic level
#'
#' @param .net network_metrics
compute_weighted_avg_trophic_level <- function(
  .net = NULL,
  bm_var = "biomass",
  .op = NULL) {

  obs_troph_level <- network_metrics %>%
    select(opcod, obs_troph_level) %>%
    mutate(obs_troph_level = map(obs_troph_level, enframe)) %>%
    unnest(obs_troph_level)

  composition <- .net %>%
    select(opcod, composition) %>%
    unnest(composition)


  stopifnot(var_chr %in% c("sp_class", "species"))
  if ("sp_class" %in% names(composition)) {
    var_chr <- "sp_class" 
  } else {
    var_chr <- "species"
  }

  stopifnot(bm_var %in% c("biomass", "bm_std"))
  if (bm_var == "bm_std") {
    stopifnot(!is.null(.op))
  }


  if (bm_var == "bm_std" & !"bm_std" %in% names(composition)) {

    sampling_effort <- dplyr::select(.op, opcod, surface)

    composition %<>%
      dplyr::left_join(sampling_effort, by = "opcod") %>%
      mutate(bm_std = biomass / surface)
  
  }


  # Renaming columns
  obs_troph_level %<>%
    rename(obs_troph_level = value, !!var_chr := name)

  bm_var <- sym(bm_var)

  weighted_mean_trophic_lvl <- composition %>%
    left_join(obs_troph_level, by = c("opcod", var_chr)) %>%
    group_by(opcod) %>%
    summarise(w_trph_lvl_avg = sum(obs_troph_level * !!bm_var) / sum(!!bm_var))

  return(weighted_mean_trophic_lvl)
}
