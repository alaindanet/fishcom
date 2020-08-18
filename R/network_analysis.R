
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



#' Calc weighted connectance 
#' @param network a matrix 
#' @references https://link.springer.com/article/10.1007/s12080-015-0291-7
calc_weighted_connectance <- function (network = NULL) {

  if (!(class(network) %in% c("dgCMatrix", "matrix"))) {
    return(NA)
  }

  if (class(network) != "matrix") {
    network <- as.matrix(network)
    network[network == 0] <- NA # To manage log
  }

  # Shannon
  total_fluxes <- sum(network, na.rm = TRUE)
  weighted_fluxes <- network / total_fluxes
  shannon <- - sum(weighted_fluxes * log(weighted_fluxes), na.rm = TRUE)
  #return(list(total_fluxes, weighted_fluxes, shannon))

  # Average mutual information
  Ti <- rowSums(network, na.rm = TRUE)
  Ti[Ti == 0] <- NA # To manage log
  Tj <- colSums(network, na.rm = TRUE)
  Tj[Tj == 0] <- NA # To manage log
  A  <- sum(weighted_fluxes * log( (network * total_fluxes) / (Ti * Tj) ), na.rm = TRUE)

  # Effective connectance per node
  m <- exp((shannon - A) / 2)

  connectance <- m / nrow(network)
  return(connectance)
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
  
  # If no piscivory in the network 
  if (all(network == 0)) return(NA)

  # Ensure ordering
  # If one species has no interaction, she is not in the network
  #stopifnot(nrow(.data) == nrow(network))
  stopifnot(all(rownames(data) %in% colnames(network)))
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
  #2. For each predator, get relative of its prey 
  .data %<>% 
    dplyr::group_by(sp_class) %>%
    dplyr::summarise(
      bm_prod = sum(bm_prod),
      nind = dplyr::n()
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


