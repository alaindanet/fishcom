
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

#' Get fish-fish adjacency matrix 
#' @param network a df with the "from" and "to" columns
get_fish_fish_adj_matrix <- function (network = NULL) {

  stopifnot("data.frame" %in% class(network))

  # Get the matrix
  out <- igraph::graph_from_data_frame(network) %>%
    igraph::as_adjacency_matrix()

  # Only keep the fish species:
  fish_species <- stringr::str_extract(colnames(out), "[A-Z]{3}_.")
  fish_species_mask <- which(colnames(out) %in% fish_species)

  filtered <- out[fish_species_mask, fish_species_mask] 

  return(filtered)


}

#' Get fish-fish adjacency matrix 
#'
#' @param  df output from get_predator_biomass_demand()
get_weighted_fish_fish_adj_matrix <- function (.data = NULL, network = NULL) {
  
  # Ensure ordering
  stopifnot(nrow(.data) == nrow(network))
  stopifnot(rownames(network) == colnames(network))
  .data <- .data[match(rownames(network), .data$sp_class), ] 

  for (i in seq_along(colnames(network))) {

    tmp_pred <- network[,i]

    # If no piscivory at all 
    if (all(tmp_pred == 0)) { next }

    #prey names
    prey <- colnames(tmp_pred)[tmp_pred != 0]

    # get interaction strength:

    tmp_pred[tmp_pred != 0] <- 
      .data[i, ]$bm_prod *
      .data[tmp_pred != 0, ]$nind / sum(.data[tmp_pred != 0, ]$nind)

    network[,i] <- tmp_pred
  }
  return(network)
}

#' Get predator biomass demand 
#'
#' @param data df with species, class and body size 
get_predator_biomass_demand <- function (.data = NULL) {

  stopifnot(all(c("species", "class_id", "length") %in% colnames(.data)))

  .data %<>%
    tidyr::unite(col = "sp_class", species, class_id) %>%
    dplyr::mutate(
      weight = calc_fish_body_mass(length),
      bm_prod = calc_biomass_production(weight)
    )

  #1. Production of biomass by population
  bm_prod_pop <- .data %>% 
    dplyr::group_by(sp_class) %>%
    summarise(bm_prod = sum(bm_prod))

  prod_pred <- bm_prod_pop[["bm_prod"]]
  names(prod_pred) <- bm_prod_pop[["sp_class"]]

  #2. For each predator, get relative of its prey 
  .data %<>% 
    dplyr::group_by(sp_class) %>%
    dplyr::summarise(
      bm_prod = sum(bm_prod),
      nind = n()
    )

  return(.data)
}

#' Calc biomass production from body mass
#' 
#' @param weight numeric body mass in grams 
#' @references Brown, J. H., Gillooly, J. F., Allen, A. P., Savage, V. M., &
#' West, G. B. (2004). Toward a metabolic theory of ecology. Ecology, 85(7),
#' 1771–1789.
calc_biomass_production <- function (weight = NULL) {
  exp(25.22 + log(weight) * 0.76)
}
