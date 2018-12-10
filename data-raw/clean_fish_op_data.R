################################################################################
#                       Clean fishing operation dataset                        #
################################################################################

# Adapted from the script of Willem Bonnafé:
# ../bonnafe_work/data/raw_data/cleanData.r

library(tidyverse)
library(magrittr)



## Sample dataset
fish_op <- read_csv("../data-raw/fishing_operation_test.csv")


############################
#  User friendly variable  #
############################
colnames(fish_op) <- str_to_lower(colnames(fish_op))
fish_op %<>%
  rowid_to_column() %>% # to escape duplicate rows problem: https://github.com/tidyverse/tidyr/issues/426
  gather(var_to_convert, values, method, strategy, bassin) %>%
  mutate(values = str_replace_all(values, " ", "_")) %>%
  spread(var_to_convert, values)

##############################
#  Remove migratory species  #
##############################
fish_op %<>% dplyr::filter(!(species %in% c("SAT", "LPP", "LPR")))

####################
#  Compute weight  #
####################
# Contains allometry relationship parameters (a & b) between body size and body mass for all fishes  
allometry_data <- read_delim("../bonnafe_work/data/raw_data/W-L_alain.csv",
  delim = ";", locale = locale("fr", decimal_mark = ".")) %>%
  select(-source) %>%
  rename(species = species_code)

## What is the unit ?  
fish_op %<>% left_join(allometry_data) %>%
  mutate(weight = a*(length^b))

# Do not register unknown weight units 
fish_op %<>% select(-weight, -a, -b)
devtools::use_data(fish_op)

#######################
#  Separate datasets  #
#######################
# TODO: file summary of fish operation: data count, nb species, etc
# To load only some columns of the dataset: https://stats.stackexchange.com/a/16798 
# Load only some columns of the dataset thanks to their order:
#../bonnafe_work/data/raw_data/Codes_colonne.txt

columns_meaning <- read_delim("../bonnafe_work/data/raw_data/Codes_colonne.txt",
  delim = ":", locale = locale("fr"), trim_ws = TRUE)
columns_meaning$var_name
columns_meaning$meaning

library('data.table')
fish_length <- fread("../bonnafe_work/data/raw_data/data_clean.txt",
  select=c("OPCOD", "Species", "Length")) %>%
  as.tibble()
devtools::use_data(fish_length)
rm(fish_length)

operation_data <- fread("../bonnafe_work/data/raw_data/data_clean.txt",
  select=c("OPCOD", "Station", "Species", "Month", "Year", "Nbpass", "Width", "Surf", "Method", "Strategy", "Count")) %>%
  as.tibble()
devtools::use_data(operation_data)
rm(operation_data)

environmental_data <- fread("../bonnafe_work/data/raw_data/data_clean.txt",
  select=c("OPCOD", "TempMF1", "TempMF2", "SumMF1", "SumMF2", "Alt", "Slope", "Distsource", "SurfBV", "Bassin")) %>%
  as.tibble()
devtools::use_data(environmental_data)
rm(environmental_data)
