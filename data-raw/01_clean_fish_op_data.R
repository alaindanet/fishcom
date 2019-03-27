################################################################################
#                       Clean fishing operation dataset                        #
################################################################################

# Adapted from the script of Willem Bonnafé:
# ../bonnafe_work/data/raw_data/cleanData.r

library(tidyverse)
library(magrittr)
library('data.table')
library('RPostgres')

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "afb_fish", port = 5434)
dbListObjects(con)
dbGetInfo(con)
dbListResults()
dbListFields()
# Not working:
dbListTables(con, schema = DBI::Id(schema="aspe"))
# But:
dbIsValid(con)
con
DBI::Id(schema="aspe")

library(dbplyr)

aspe_tables <- dbListObjects(con, prefix = DBI::Id(schema="aspe"))$table
db_station <- tbl(con, in_schema("aspe", "station"))
select(db_station, sta_geometrie)
colnames(db_station)

db_op <- tbl(con, in_schema("aspe", "operation"))
db_prelevement <- tbl(con, in_schema("aspe", "prelevement_elementaire"))
db_lot <- tbl(con, in_schema("aspe", "lot_poissons"))
db_mesure <- tbl(con, in_schema("aspe", "mesure_individuelle"))

#################################
#  Build the operation dataset  #
#################################

## get the id:
obj_to_keep <- "RRP|RCS|CO|RNSORMCE|Étude|restauration"
obj_id <- tbl(con, in_schema("aspe", "ref_objectif")) %>%
  collect() %>%
  filter(str_detect(obj_libelle, obj_to_keep)) %>%
  select(obj_id) %>% unlist

## Select the op: 
op_id <- tbl(con, in_schema("aspe", "operation_objectif")) %>%
  collect %>%
  filter(opo_obj_id %in% obj_id) %>%
  select(opo_ope_id) %>% unlist(., use.names = FALSE) 
op <- db_op %>%
  filter(ope_id %in% op_id)

## Create the dataset: 
op %<>%
  rename_all(funs(gsub("ope_", "", make.names(colnames(op)))))
#colnames(op)
col_to_keep <- c("id", "date", "pente_ligne_eau", "section_mouillee",
  "durete_totale", "temp_max_moyenne_eau", "temp_air_bassin_versant",
  "precipitation_bassin_versant", "amplitude_thermique_air_station",
  "temperature_air_station", "surface_calculee", "espece_ciblee", "niq_id",
  "eta_id", "commentaire")
### filter for qualified fishing operation:
op %>%
  arrange(desc(date))
unique(op_test$niq_id)

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
fish_op <- fread("../bonnafe_work/data/raw_data/NEW_DATA.txt")
colnames(fish_op) <- str_to_lower(colnames(fish_op))
write_csv(fish_op[!(fish_op$species %in% c("SAT", "LPP", "LPR"))],
  "global_fish_op.csv")
rm(fish_op)

## Contains only the minimum information: code of the capture, species, body
## length 
fish_length <- fread("global_fish_op.csv",
  select = c("opcod", "species", "length")) %>%
  as.tibble()
# keyboard error
fish_length %<>%
  mutate(
    length = replace(length, length < 1 & species == "TRF", 77)
  )
devtools::use_data(fish_length, overwrite = TRUE)
rm(fish_length)

operation_data <- fread("global_fish_op.csv",
  select=c("opcod", "station", "species", "month", "year", "nbpass", "width", "surf", "method", "strategy", "count")) %>%
  as.tibble()
operation_data %<>%
  mutate(
    method = str_replace_all(method,
      c("partielle sur be" = "partial_over_be",
	"Point grand mil"  = "point_big_mil",
	"partielle sur to"  = "partial_over_to",
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

# Remove spurious data:
## nbpass is irrevelant for method != complete
operation_data %<>% mutate(
  nbpass = replace(nbpass, method != "complete", NA),
  surf = ifelse(surf == 0, NA, surf)# if sampled area is 0, there is a problem
)
#RQ pour pêches partielles par ambiance et par point, par défaut on a supprimé
#toutes les valeurs qui pouvaient avoir été indiquées de façon à ce que la
#cellule apparaisse vide- ; pour les autres (berge, autre) on a laissé les
#valeurs même si elles sont contradictoire avec la méthode


devtools::use_data(operation_data, overwrite = TRUE)
rm(operation_data)

# Environmental data
environmental_data <- fread("global_fish_op.csv",
  select=c("opcod", "tempmf1", "tempmf2", "summf1", "summf2", "alt", "slope", "distsource", "surfbv", "bassin"))
# Normaly, we have one unique data by opcod, i.e. one by fishing operation:
is_unique <- environmental_data %>%
  dplyr::select(opcod, tempmf1, tempmf2, summf1, summf2) %>% # Check for highly details variable values, because duplicate artefacts are less probable 
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

############################################################
#  Check that data_clean of Guillem is the same than mine  #
############################################################


##Let's check how many fishing operation there is in data_clean of bonnafé and
##compare them to what we have
data(environmental_data)
opcod_to_compare <- environmental_data %>%
  distinct(opcod)
library(data.table)
opcod_willem <- fread("../bonnafe_work/data/raw_data/data_clean.txt",
  select = c("OPCOD")) %>%
  as.tibble() %>%
  rename("opcod" = "OPCOD") %>%
  distinct(opcod)
##It's ok:
all_equal(opcod_willem, opcod_to_compare)

# Cleaning done! +1 for the reproducibility!
