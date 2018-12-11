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
allometry_data <- read_delim("../bonnafe_work/data/raw_data/W-L.csv",
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

## Clean 
library('data.table')
fish_op <- fread("../bonnafe_work/data/raw_data/NEW_DATA.txt")
colnames(fish_op) <- str_to_lower(colnames(fish_op))
write_csv(fish_op[species != c("SAT", "LPP", "LPR")],
  "global_fish_op.csv")

## Contains only the minimum information: code of the capture, species, body
## length 
fish_length <- fread("global_fish_op.csv",
  select = c("opcod", "species", "length")) %>%
  as.tibble()
devtools::use_data(fish_length, overwrite = TRUE)
rm(fish_length)

operation_data <- fread("global_fish_op.csv",
  select=c("opcod", "station", "species", "month", "year", "nbpass", "width", "surf", "method", "strategy", "count")) %>%
  as.tibble()
operation_data %<>%
  mutate(
    method = str_replace_all(method,
      c("partielle sur be" = "partial_over_be",
	"partielle sur be" = "partial_over_to",
	"Point grand mil"  = "point_big_mil",
	"autres"           = "other")),
    strategy = str_replace_all(strategy,
      c("A pied"        = "on_foot",
	"En bateau"     = "by_boat",
	"Mixte"         = "mixed",
	"Non renseigne" = "unknown")
    )
)
# One data by species and fishing operation 
operation_data %<>% distinct(opcod, species, .keep_all = TRUE)
devtools::use_data(operation_data, overwrite = TRUE)
rm(operation_data)

# Environmental data
environmental_data <- fread("global_fish_op.csv",
  select=c("opcod", "tempmf1", "tempmf2", "summf1", "summf2", "alt", "slope", "distsource", "surfbv", "bassin"))
# Normaly, we have one unique data by opcod, i.e. one by fishing operation:
is_unique <- environmental_data %>%
  select(opcod, tempmf1, tempmf2, summf1, summf2) %>% # Check for highly details variable values, because duplicate artefacts are less probable 
  gather(variable, values, tempmf1, tempmf2, summf1, summf2) %>%
  group_by(opcod, variable) %>%
  summarise(n_unique = length(unique(values)))
#> length(which(is_unique$n_unique > 1))
#[1] 0
# Good, let remove duplicate:
environmental_data %<>% distinct(opcod, .keep_all = TRUE)

#Some data contains far too much decimals:
#slice(environmental_data, 1)
#opcod tempmf1 tempmf2 summf1 summf2 alt slope distsource surfbv bassin
#3 10.16415 10.18481 121.9698 244.4355 38 1.6 1686 Seine
environmental_data %<>%
  as.tibble() %>%
  mutate(
    tempmf1 = round(tempmf1, 1),
    tempmf2 = round(tempmf2, 1),
    summf1 = round(summf1),
    summf2 = round(summf2)
    )
# Let fix the name of the basins:
environmental_data %<>%
  as.tibble() %>%
  mutate(
    bassin = str_replace_all(bassin,
      c("Seine"                    = "seine",
	"CatiersManche"            = "manche",
	"BassinsNord(Rhin, Meuse)" = "nord",
	"Catiersatlantiques"       = "atlantique",
	"Loire"                    = "loire",
	"Nonrenseigna"             = "unknown",
	"Rhane"                    = "rhone",
	"Garonne"                  = "garonne",
	"Catiersmaditerranaens"    = "mediterranee"))
    )

devtools::use_data(environmental_data, overwrite = TRUE)
rm(environmental_data)
