################################################################################
#                      Functions to build local networks                       #
################################################################################

#' Local network building
#' 
#' 
#' @param metaweb an object created by the build_metaweb function.
#' @inheritParams get_size_class 
#' @param group_var variable characterizing different communities.
#'
#' @details 
#'
#' @return data.frame containing local network code and interaction matrix.
#'
build_local_network <- function (metaweb, classes, data, species, var, group_var) {
  
  species <- rlang::enquo(species)
  var <- rlang::enquo(var)
  group_var <- rlang::enquo(group_var)

  #Attribute size class for each fish
  classes_assigned <- assign_size_class(data, !!species, !!var, classes)

  resource_list <- metaweb$resource_list
}
#' Assign size classes, for all species 
#' 
#' @param data a data.frame containing species and size variable.
#' @param species species variable name in the dataset.
#' @param var variable characterizing the size.
#' @param classes data.frame created by the compute_classes function.
#'
#' @return data.frame containing class_id for each indivual.
#'
assign_size_class <- function (data, species, var, classes) {
  
  species <- rlang::enquo(species)
  var <- rlang::enquo(var)

  #Attribute size class for each fish
  classes_assigned <- data %>% dplyr::group_by(!!species) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      class_id = purrr::pmap(list(data = data, species_name = !!species),
	get_size_class, var = !!var, classes = classes)
    )

  classes_assigned %>% tidyr::unnest() %>%
    dplyr::select(-!!var) %>%
    mutate(class_id = as.integer(class_id))
}
#' Get size classes for a given species 
#' 
#' @param data a data.frame containing species and size variable.
#' @param species data.frame containing species and the predation window.
#' @param var variable characterizing the size.
#' @param classes data.frame created by the compute_classes function.
#'
#' @return data.frame containing local network code and interaction matrix.
#'
get_size_class <- function (data, species_name, var, classes) {
  
  species_name <- rlang::enquo(species_name)
  var <- rlang::enquo(var)

  #Get good classes
  classes %<>% dplyr::filter(species == rlang::quo_name(species_name)) %>%
  dplyr::select(lower, upper)

  #Attribute size class for each fish
  data %<>% dplyr::select(!!var) %>% unlist(.)

  # Use findInterval
  # see https://rpubs.com/josephuses626/findInterval to improve
  mat_match <- apply(classes, 1, findInterval, x = data, left.open = TRUE) == 1
  #correct for first interval which is left close:
  mat_match[1,1] <- TRUE
  apply(mat_match, 1, function(x) which(x)) %>% unlist(.)
}
