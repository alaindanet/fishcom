#' Diet overlap between consumers
#'
#' Computes the diet overlap between pairs of consumers.
#'
#' @param adj an adjacency matrix
#' @details 
#' @return a data.frame with overlap value by species pairs 
#' @references Pianka, E. R. (1973). The Structure of Lizard Communities. Annual
#' Review of Ecology and Systematics, 4(1), 53‑74.
#' https://doi.org/10.1146/annurev.es.04.110173.000413
compute_diet_overlap <- function (adj) {
  if (is.null(colnames(adj))) {
    colnames(adj) <- seq(1, ncol(adj))
  }
  sp_pairs <- combn(colnames(adj), 2, simplify = FALSE)

  overlap <- purrr::map_dbl(sp_pairs, function(sp_pairs, adj) {
    sp <- adj[, sp_pairs]
    compute_two_species_overlap(sp[, 1], sp[, 2])
}
    , adj = adj)
  names(overlap) <- purrr::map_chr(sp_pairs,
    function(x) paste("O", x[1], "_", x[2], sep = ""))
  overlap
}

#' Diet overlap between two consumer
#' @param sp1 vector of interaction
#' @param sp2 vector of interaction
compute_two_species_overlap <- function (sp1, sp2) {

  sp_names <- c("sp1", "sp2")
  sp <- list(sp1, sp2)
  names(sp) <- sp_names
  nb_res <- purrr::map_dbl(sp, sum)

  # Resource is used in which proportion:
  p <- purrr::map2_dfr(sp, nb_res, function(int, res) int / res)
  names(p) <- sp_names

  sum_p_squared <- purrr::map_dbl(p, function(diet) {sum(diet^2)})
  names(sum_p_squared) <- sp_names

  overlap <- sum(p$sp1 * p$sp2) /
    sqrt(sum_p_squared["sp1"] * sum_p_squared["sp2"])

  names(overlap) <- NULL
  return(overlap)
}

