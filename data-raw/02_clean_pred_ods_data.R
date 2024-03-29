################################################################################
#           Cleaning predation window data and ontogenic diet shifts           #
################################################################################
library('tidyverse')
library('magrittr')


###############
#  Load data  #
###############
fish_diet_shift <- read_delim("ontogenic_diet_shift_fish.csv",
  delim = ";", locale = locale(decimal_mark = "."))
resource_diet_shift <- read_delim("ontogenic_diet_shift_resource.csv",
  delim = ";", locale = locale(decimal_mark = "."))
pred_win <- read_delim("fish_fish_predation_window.csv",
  delim = ";", locale = locale(decimal_mark = "."))

####################
#  Clean and save  #
####################
colnames(pred_win) <- str_replace_all(colnames(pred_win),
  c("par_" = "", "1" = "alpha", "2" = "beta", "_code" = ""))
devtools::use_data(pred_win, overwrite = TRUE)

colnames(resource_diet_shift) <- str_replace_all(colnames(resource_diet_shift),
  c("species" = "species_name", "species_name_code" = "species"))
devtools::use_data(resource_diet_shift, overwrite = TRUE)

colnames(fish_diet_shift) <- str_replace_all(colnames(fish_diet_shift),
  c("species" = "species_name", "species_name_code" = "species"))
fish_diet_shift %<>% mutate(size_max = str_replace_all(size_max,
    c("max" = "Inf")),
  size_max = as.numeric(size_max))
devtools::use_data(fish_diet_shift, overwrite = TRUE)
