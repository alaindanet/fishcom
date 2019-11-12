
#' Compute betadiv
#'
#'
#'@export
compute_betadiv <- function(com, binary = FALSE, time_to_time = FALSE) {
  if (binary) {
    com %<>% replace(., . != 0, 1)
  }

  dist_mat <- vegan::vegdist(com, method = "bray", binary = binary)

  if (time_to_time) {
    dist_mat <- diag(dist_mat)
  }

  mean(dist_mat)
}
