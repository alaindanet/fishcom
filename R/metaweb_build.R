###########################################
#  Functions necessary to create metaweb  #
###########################################

#' Metaweb building
#' 
#'
#' @param pred_win data.frame containing species and the predation window
#'  parameters (\alpha and \beta).
#' @param diet_shift data.frame containing species and the predation window.
#' @param pred_win_method character midpoint, overlap or no_overlap. Default to midpoint.
#' @inheritParams compute_classes 
#' @details class_method = "quantile" use the `quantile` function to create size
#'  class in nb_class . When class_method = "percentile", the range of the size distribution
#'  is divided by nb_class. The difference is that `quantile` start the first
#'  class at the minimum value of the size distribution while percentile begins
#'  at 0. It therefore gives slightly different results.
#'  When the predation window definition is defined as overlap or midpoint, there
#'  will be possibility of overlap between size class of a predator and a prey.
#'  It means that for trophic link established between a prey and a predator, the
#'  smallest predator of a given size class is smaller than the biggest prey
#'  fish. When pred_win_method is set to "no_overlap", the upper limit of the predation window of
#'  a predator size class is defined by the smallest predator of the size class,
#'  so there is no overlap between predator and prey. With midpoint option, the
#'  existence of a trophic link is defined by the middle of the predator size
#'  class.
#'
#' @return 
#'
build_metaweb <- function(size, pred_win, diet_shift, class_method = "percentile",
  nb_class = 9, pred_win_method = "midpoint") {


}

#' Compute classes
#'
#' @param size data.frame containing species and length.
#' @inheritParams split_in_classes 
#'
#' @return
compute_classes <- function(size, group_var, var, class_method = "percentile",
  nb_class = 9) {

  stopifnot(class_method %in% c("percentile", "quantile"))
  stopifnot(is.numeric(nb_class))

  #Capture var:
  group_var <- enquo(group_var)
  var <- enquo(var)

  nested_size <- size %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::select(!!group_var, !!var) %>% #ensure that there is only the varialble of interest
    tidyr::nest()
  nested_size %<>%
    dplyr::mutate(
      classes = purrr::map(data, unlist),
      classes = purrr::map(classes, split_in_classes, nb_class = nb_class, class_method = class_method)) %>%
  select(-data)

nested_size %>%
  tidyr::unnest(classes)
    
      #classes = map(classes, ~ bind_rows(.))
    #)%>%
    #unnest()

}

#' Split in classes
#'
#' @param class_method character percentile or quantile. Default to percentile. 
#' @param nb_class integer number of size class to create. Default to 9. 
  split_in_classes <- function (to_class, class_method = "percentile",
    nb_class = 9) {

    if (class_method == "quantile") {
      classified <- quantile(to_class, probs = seq(0, 1, by = 1 / nb_class))
    } else {
      classified <- seq(0, max(to_class), by = ( (max(to_class) - 0) / (nb_class)))
    }
    classified %<>% round

    # Determine the lower and upper limit of each size class:
    tibble::tibble(
      class_id = seq(1, nb_class),
      lower = classified[-length(classified)],
      upper = classified[-1]
	)
  }
