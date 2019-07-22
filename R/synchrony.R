################################################################################
#                        Functions to compute synchrony                        #
################################################################################
compute_avg_cv_sp <- function(biomass, variance) {
  #Check that the species are in the same order in the vector:
  stopifnot(names(biomass) == names(variance))

  rel_biomass <- biomass / sum(biomass)
  rel_sdt <- sqrt(variance) / biomass

  cv_avg <- sum(rel_biomass * rel_sdt)
  return(cv_avg)
}

compute_cv_com <- function(synchrony, cv_sp) {
  sqrt(synchrony) * cv_sp
}

compute_synchrony <- function(cov_mat) {
  com_var <- sum(cov_mat)
  var_intra_sp <- sum(sqrt(diag(cov_mat)))
  phi <- com_var / (var_intra_sp^2)
  return(phi)
}
