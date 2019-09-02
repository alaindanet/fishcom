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

