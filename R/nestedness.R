#' Compute nestedness
#' 
#' Compute the nestedness of a network according to Bastolla et al. (2009).
#' @param adj an adjacency matrix
#' @details The nestedness is computed as follows: \eqn{\nu(A) = \frac{\sum
#' \nu_{i,j}}{\sum min(\nu_i, \nu_j)}}
#' Where 
#' @references Bastolla, U., Fortuna, M. A., Pascual-García, A., Ferrera, A., Luque, B., & Bascompte, J. (2009). The architecture of mutualistic networks minimizes competition and increases biodiversity. Nature, 458(7241), 1018‑1020. https://doi.org/10.1038/nature07950
#'
#' @return double. The nestedness value
#' @export
nestedness <- function (adj) {

  stopifnot(all(colnames(adj) == rownames(adj)))
  if (any(adj > 1)) {
    adj[which(adj) > 1] <- 1
  }
  species_comb <- combn(colnames(adj), 2) %>% as.data.frame %>% t
  sum_interaction <- purrr::map2_dfr(species_comb[,1], species_comb[,2], function(sp1, sp2){
    sp1 <- adj[, sp1]
    sp2 <- adj[, sp2]

    nb_common_interaction <- length(which(sp1 & sp2))
    min_nb_interaction <- pmin(sum(sp1), sum(sp2))

    list(nij = nb_common_interaction, min_ni_nj = min_nb_interaction)
})
  sum_interaction <- colSums(sum_interaction) %>% unlist
  nestedness <- sum_interaction["nij"] / sum_interaction["min_ni_nj"]
  if (is.nan(nestedness)) {
    message("The matrix contains only 0, nestedness is NaN")
  }

  names(nestedness) <- NULL
  nestedness
}
