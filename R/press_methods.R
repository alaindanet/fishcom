################################################################################
#                               Manipulate press                               #
################################################################################

gather_press_to_category <- function(press = NULL, polluant_units = NULL, var_to_sum = NULL, group_var = NULL, ld_weighting = TRUE) {

  # Polluants avg by those categories and weighted by their ld_50:
  ld_cat <- c(
    "herbicides",
    "insecticides",
    "fungicides",
    "pcb",
    "hap",
    "micropolluants mineraux",
    "micropolluants organiques")

  # LD50 are in mg/L and parameter in µg/L:
  press %<>%
    left_join(polluant_units) %>%
    select(parameter, direction, ld50_fish, units, !!var_to_sum, !!group_var)

  #########################
  #  Weighting polluants  #
  #########################

  press_ld <- press %>%
    filter(category %in% ld_cat & !is.na(ld50_fish)) 
  stopifnot(unique(press_ld$units) == "µg/L")

  # Weighted sum of ld by station:
  press_ld %<>%
    mutate(ld50_fish = ld50_fish * 10^3) %>%
    # Get ld_50 in µg. no effect since it's a weight
    # But it's to remember the unit issue
    group_by_at(vars(group_var)) 

  if (ld_weighting) {
    press_ld %<>%
      summarise_at(var_to_sum,
	~sum(. * (1 / ld50_fish / sum(1 / ld50_fish, na.rm = TRUE)),
	  na.rm = TRUE)
      )
  } else {
    press_ld %<>%
      summarise_at(var_to_sum,
	~sum(., na.rm = TRUE)
      )
  }


  #######################
  #  Parameter non weighted  #
  #######################

  press_non_ld <- press %>%
    filter(!category %in% ld_cat)
  cat_to_z_press <- c(
    "matieres azotees",
    "matieres organiques",
    "mes", "matieres phosphorees",
    "industry", "other"
  ) 

  press_non_ld %<>%
    group_by(parameter) %>%
    mutate(z_press_temp = ifelse(category %in% cat_to_z_press, scale(press), press)) %>%
    mutate(z_press = pmap_dbl(list(category, direction, z_press_temp),
	function(cate, direction, z){# take care of the direction of the gradient
	  if (!cate %in% cat_to_z_press) {
	    return(z) 
	  }
	  if (direction == "decreasing") {
	    z <- z * -1
	    return(z)
	  } else {
	    return(z)
	  }
	}))
    #test_inversion <- filter(press_non_ld, parameter == "ph")
    #stopifnot(test_inversion[1,]$z_press_temp == test_inversion[1,]$z_press * -1)

    press_non_ld %<>%
      select(group_var, parameter, var_to_sum, z_press)

    press_non_ld %<>%
      group_by_at(vars(group_var)) %>%
      summarise_at(var_to_sum, sum)

    #Merge the two:
    press <- bind_rows(press_ld, press_non_ld) %>%
      as_tibble() %>%
      unnest()
    press
}

#' Format press
#'
#'
#'
format_press_polluant <- function (press = NULL, polluant_units = NULL, press_cat = NULL) {

  press %<>%
    dplyr::group_by(id, year, parameter) %>%
    dplyr::arrange(value.predSE) %>%
    dplyr::slice(1)


  press %<>%
    dplyr::ungroup() %>%
    dplyr::mutate(parameter = as.character(parameter)) %>%
    dplyr::left_join(press_cat, by = "parameter")
  missing_cat <- dplyr::filter(press, is.na(category))
  stopifnot(nrow(missing_cat) == 0)
  # Move aluminium which is more a polluant than acidification
  press %<>%
    dplyr::mutate(
      category = ifelse(parameter == "aluminium", "micropolluants mineraux", category),
      category = ifelse(parameter == "ph", "ph", category) 
    )
  # Keep only phosphore total:
  press %<>%
    dplyr::mutate(
      category = ifelse(parameter == "phosphore_total", "phosphore", category)
    )
  # Matières organiques
  press %<>%
    dplyr::mutate(
      category = ifelse(parameter == "dbo5", "DBO", category),
      category = ifelse(parameter == "oxygene_dissous", "disolved_oxygen", category)
    )

  press_cleaned <- press %>%
    dplyr::select(id, year, parameter, value_corrected, direction, category, ld50_fish) %>%
    dplyr::rename(press = value_corrected)

  press_metrics <- gather_press_to_category(
    press = press_cleaned,
    polluant_units = polluant_units,
    var_to_sum = c("press"),
    group_var = c("id", "category", "year"))

  return(press_metrics)

}

#' Get temperature and flow metrics
#'
#'
get_temp_flow_metrics <- function (flow = NULL, temp = NULL) {

  temp_flow_temp <- flow %>%
    dplyr::mutate(category = "flow") %>%
    dplyr::bind_rows(dplyr::mutate(temp, category = "temperature")) %>%
    select(id, year, category,value_corrected) %>%
    rename(press = value_corrected)

  return(temp_flow_temp)

}

compute_temporal_press <- function (press_metrics = NULL, temp_flow_metrics = NULL, .op = NULL) {

  year_station <- .op %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(year_list = list(unique(year)))


  # Put temperature aside bc of temperature data is begining in 2006:
  # Summarise data over 2006-2018 
  temp <- dplyr::filter(temp_flow_metrics, category == "temperature")
  temporal_temp <- temp %>% 
    dplyr::filter(id %in% year_station$station) %>%
    dplyr::group_by(id, category) %>%
    dplyr::summarise(
      press_med = median(press, na.rm = TRUE),
      cv_press = sd(press, na.rm = TRUE) / mean(press, na.rm = TRUE),
      press = mean(press, na.rm = TRUE)
    )

  press_metrics %<>%
    dplyr::bind_rows(dplyr::filter(temp_flow_metrics, category != "temperature"))

  # Select press years corresponding to years sampled in press metrics
  press_metrics %<>%
    dplyr::filter(id %in% year_station$station) %>%
    dplyr::group_by(id) %>%
    tidyr::nest() %>%
    dplyr::left_join(dplyr::rename(year_station, id = station), by = "id") %>%
    dplyr::mutate(sorted = purrr::map2(data, year_list, function (x, year_list) {
	dplyr::filter(x, year %in% c(min(year_list) - 1, year_list))
	})) %>%
    dplyr::select(-data, -year_list) %>%
    tidyr::unnest(sorted)

  temporal_press_polluants <- press_metrics %>% 
    dplyr::group_by(id, category) %>%
    dplyr::summarise(
      press_med = median(press, na.rm = TRUE),
      cv_press = sd(press, na.rm = TRUE) / mean(press, na.rm = TRUE),
      press = mean(press, na.rm = TRUE)
    )

  return(temporal_press_polluants)

}

compute_temporal_habitat <- function (.op = NULL ) {


}

#' Built the habitat and press dataset 
#'
#' @param .habitat_analysis data.frame 
#' @param .tmp_press data.frame
#' @param .tmp_st_desc data.frame
#' @param .geo_st data.frame
#'
build_habitat_pressure_dataset <- function (
  .habitat_analysis = NULL,
  .tmp_press = NULL,
  .tmp_st_desc = NULL,
  .geo_st = NULL
    ) {

  # Transform the qualitative data from character to ordered factor
  habitat <- .habitat_analysis %>%
    dplyr::mutate_all(list(as.factor)) %>%
    dplyr::select(-opcod, -station, -aqua_vg)

  lvl <- c("null", "weak", "medium", "high")
  lvl_sinuosite <- c("straight", "sinuous", "very_sinuous", "meandering")
  lvl_shade <- c("clear", "quite_clear", "quite_covered", "covered")

  habitat_station <- .habitat_analysis %>%
    dplyr::select(-opcod, -aqua_vg, - shade, - sinuosite) %>%
    dplyr::mutate_if(is.character, list(~factor(., levels = lvl, ordered = TRUE))) %>%
    dplyr::left_join(dplyr::select(hab_analysis, station, shade, sinuosite)) %>%
    dplyr::mutate(
      shade = factor(shade, levels = lvl_shade, ordered = TRUE),
      sinuosite = factor(sinuosite, levels = lvl_sinuosite, ordered = TRUE)
    )

  # Compute avg over the studied period 
  habitat_station %<>%
    group_by(station) %>%
    dplyr::summarise_all(list(med = ~median(as.numeric(.), na.rm = TRUE)))


  # Add alt, lat, etc...
  habitat_station %<>% 
    dplyr::left_join(.geo_st, by = "station") %>%
    dplyr::select(-region_csp) %>%
    dplyr::left_join(dplyr::select(.tmp_st_desc, station, width_river_mean,
	avg_depth_station_mean, width_river_cv, avg_depth_station_cv))

  # Add temp and flow
  flow_temp <- .tmp_press %>%
    dplyr::filter(category %in% c("flow", "temperature", "DBO", "herbicides",
	"fungicides", "insecticides", "nitrates", "phosphore")) %>%
  dplyr::rename(station = id)

  # Get Med and cv for press
  flow_temp_med <- flow_temp %>%
    dplyr::mutate(category = str_c(category, "_med")) %>%
    dplyr::select(station, category, press_med) %>%
    tidyr::spread(category, press_med)
  flow_temp_cv <- flow_temp %>%
    dplyr::mutate(category = str_c(category, "_cv")) %>%
    dplyr::select(station, category, cv_press) %>%
    tidyr::spread(category, cv_press)

  habitat_station %<>%
    dplyr::left_join(dplyr::left_join(flow_temp_med, flow_temp_cv, by = "station"), by = "station")

  return(habitat_station)
}
