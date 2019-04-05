#' Get size from lot (AFB)
#' 
#' @param lot data.frame
#' @inheritParams gen_fish_from_lot
get_size_from_lot <- function(
  lot = NULL, id_var = NULL, type_var = NULL, nb_var = NULL,
  min_var = NULL, max_var = NULL, species = NULL,
  measure = NULL, measure_id_var = NULL, size_var = NULL,
  future_enabled = FALSE, ...){

  id_var <- rlang::enquo(id_var)
  id_var_chr <- rlang::quo_name(id_var)
  type_var <- rlang::enquo(type_var)
  type_var_chr <- rlang::quo_name(type_var)
  nb_var <- rlang::enquo(nb_var)
  nb_var_chr <- rlang::quo_name(nb_var)
  species <- rlang::enquo(species)

  max_var <- rlang::enquo(max_var)
  min_var <- rlang::enquo(min_var)
  measure_id_var <- rlang::enquo(measure_id_var)
  size_var <- rlang::enquo(size_var)



  # Filter incorrect lot:
  diff_lot_type <- c("G", "S/L", "N", "I")
  if (any(is.na(lot[[type_var_chr]]) |
      any(!lot[[type_var_chr]] %in% diff_lot_type))
    ) {
    lot %<>%
      dplyr::filter(! is.na(!!type_var) & !!type_var %in% diff_lot_type)
    message("NA lot id and lot type has been filtered")
  }

  # Filter if effectif is not present:
  if (any(is.na(lot[[nb_var_chr]])) | any(!lot[[nb_var_chr]] > 0)) {
    lot %<>%
      dplyr::filter( (!is.na(!!nb_var)) & !!nb_var > 0)
    message("Incorrect effectif has been filtered")
  }

  na_G <- lot %>%
    dplyr::filter(!!type_var == "G" & (is.na(!!min_var) | is.na(!!max_var)))
  incorrect_G <-  lot %>%
    dplyr::filter(!!type_var == "G" & !!min_var >= !!max_var)

  if (any(c(nrow(na_G), nrow(incorrect_G)) != 0)) {
    G_bad_id <- c(na_G[[id_var_chr]], incorrect_G[[id_var_chr]])

    lot %<>% dplyr::filter(!( !!id_var %in% G_bad_id))
    message("incorrect lot G have been filtered")
  }

  if (future_enabled) {
    loop <- furrr::future_pmap
  } else {
    loop <- purrr::pmap
  }
  output <- lot %>%
    dplyr::mutate(
      fish =
    loop(list(
    id = !!id_var,
    type = !!type_var,
    min_size = !!min_var,
    max_size = !!max_var,
    nb = !!nb_var
        ),
      gen_fish_from_lot,
      ind_measure = measure,
      ind_size = !!size_var,
      ind_id = !!measure_id_var
    )
      ) %>%
  dplyr::select(!!id_var, !!species, fish) %>%
  tidyr::unnest(fish)

  output

}

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
  ind_measure = NULL, ind_id = NULL, ind_size = NULL, ...) {

  # check:

  # Promise:
  ind_id <- rlang::enquo(ind_id)
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
    "min_size >= max_size in lot of type G number ",id,", lot put as NA\n",
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
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    size <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    size <- na.omit(size)
    stopifnot(is.na(size) | nrow(size) == 0)
    # Sanity check:
    if (length(size) < 30) {
      warning_msg <- paste(
      "# of obs is inferior to 30 (actual size is,",length(size),
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
    }
  } else if (type == "I") {
    # All individuals have been measured:
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    lot <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(lot) == nb)
  } else if (type == "N") {
    # One big individual:
    mask <- which(ind_measure[[rlang::quo_name(ind_id)]] == id)
    lot <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(lot) == 1)
  }
  # Round to milimeters:
  round(lot)
}
