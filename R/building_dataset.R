#' Generate fish from fishing lot (AFB) 
#'
#'
#' @param id int id of the lot  
#' @param type character type of the lot (N, G, S/L, I)
#' @param min_size dbl minimum size of the lot 
#' @param max_size dbl maximum size of the lot 
#' @param nb int effectif of the lot 
#' @param ind_measure data.frame individual measurement of the fish 
#' @param seed int for set.seed
#'
#' @details From fishing lot, we build generate the fish size individual
#'
#' @export
gen_fish_from_lot <- function (
  id = NULL, type = NULL,  min_size = NULL, max_size = NULL, nb = NULL,
  ind_measure = NULL, ind_id = NULL, ind_size = NULL, verbose = FALSE, ...) {

  if (verbose) {
    cat(sprintf("Lot number %s \n", id))
  }

  # Promise:
  ind_id <- rlang::enquo(ind_id)
  id <- rlang::enquo(id)
  ind_size <- rlang::enquo(ind_size)

  # Build by lot
  if (type == "G") {
    if (any(is.na(c(min_size, max_size)))) {
      warning_msg <- paste(
	"NA in lot of type G number ",id,", lot put as NA\n", sep = ""
      )
      warning(warning_msg)
      lot <- NA
    } else if (min_size >= max_size) {
      warning_msg <- paste(
	"min_size <= max_size in lot of type G number ",id,", lot put as NA\n",
	sep = "")
      warning(warning_msg)
      lot <- NA
    } else {
    avg <- (min_size + max_size) / 2
    sdt <- (max_size - min_size) * 1 / 4

    lot <- truncdist::rtrunc(n = nb, spec = "norm", a = min_size, b = max_size,
      mean = avg, sd = sdt)
    stopifnot(length(lot) == nb)
    }
  } else if (type == "S/L") {
    #Get size:
    size <- ind_measure %>% dplyr::filter(!!ind_id == !!id) %>%
      dplyr::select(!!ind_size) %>%
      unlist(., use.names = FALSE)
    # Sanity check:
    if (length(size != 30)) {
      warning_msg <- paste(
      "# of obs different from 30 (actual size is,",length(size),
      ") in Lot type S/L number ", id,".\n", "Lot put as NA\n", sep = "")
      warning(warning_msg)
      lot <- NA
    } else {
    #Distribution parameters:
    avg <- mean(size)
    sdt <- sd(size)

    # Sample inside the 90% of the distribution probability:
    p05 <- quantile(size, 0.05)
    p95 <- quantile(size, 0.95)
    lot <- truncdist::rtrunc(n = nb, spec = "norm", a = p05, b = p95,
      mean = avg, sd = sdt)
    stopifnot(length(lot) == nb)
    }
  } else if (type == "I") {
    # All individuals have been measured:
    lot <- ind_measure %>% dplyr::filter(!!ind_id == !!id) %>%
      dplyr::select(!!ind_size) %>%
      unlist(., use.names = FALSE)
    stopifnot(length(lot) == nb)
  } else if (type == "N") {
    # One big individual:
    lot <- ind_measure %>% dplyr::filter(!!ind_id == !!id) %>%
      dplyr::select(!!ind_size) %>%
      unlist(., use.names = FALSE)
    stopifnot(length(lot) == 1)
  }
  # Round to milimeters:
  round(lot)
}
