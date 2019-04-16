################################################################################
#                       Clean fishing operation dataset                        #
################################################################################

# Adapted from the script of Willem Bonnafé:
# ../bonnafe_work/data/raw_data/cleanData.r

library(tidyverse)
library(magrittr)
devtools::load_all()
mypath <- rprojroot::find_package_root_file
mydir <- mypath("data-raw", "fishing_op_build")

################################################################################
#                                 Fish length                                  #
################################################################################


load(mypath("data-raw", "fishing_op_build", "fish_length.rda"))


##############################
#  Remove migratory species  #
##############################
# Remove migratory species, species from lake, crayfish or very rare ones
sp_to_remove <- c(
"ALR",#Alose_feinte_du_Rhone
"ALF",#Alose_feinte	
"ALA",#Grande_alose	
"APP",#Ecrevisse_a_pieds_blancs	
"ASA",#Ecrevisse_a_pieds_rouges	
"ASL",#Ecrevisse_a_pieds_greles	
"ATH",#Atherine_pretre	
"CDR",#Crapet_de_roche
"COR",#Coregone	
"CRI",#Christivomer
"CTI",#Amour_blanc	
"FLE",#Flet	
"GOB",#Gobie	
"LOU",#Bar	
"LPM",#Lamproie_marine	
"MGL",#Mulet_a_grosses_levres	
"MUC",#Mulet_cabot	
"MUD",#Mulet_dore	
"MUP",#Mulet_porc	
"OCL",#Ecrevisse_americaine	
"PCC",#Ecrevisse_de_louisiane	
"PFL",#Ecrevisse_signal	
"PIM",#Tete_de_boule, n=1	
"PLI",#Plie	
"SCO",#Saumon_coho n=4
"SAT",#Saumon atlantique
"LPP",
"LPR",
"UMP"
)

fish_length %<>% dplyr::filter(!(species %in% sp_to_remove))

################################
#  Regroup morphotype species  #
################################
#old = new
sp_to_replace <- c(
"CMI" = "CCO", #"Carpe_miroir"<-"Carpe_commune"
"CCU" = "CCO", #"Carpe_cuir"<-"Carpe_commune"
"RUB" = "GAR", #"Gardon_italien"<-"Gardon" n=1
"CYP" = "GAR", #"Juvenile_cyp"<-"Gardon"
"CAG" = "CAS", #"Carassin_argente"<-"Carassin"
"CAA" = "CAS", #Carassin_dore_ou_argente"<-"Carassin"
"CAR" = "CAS", #"Carpe_argentee"<-"Carassin"
"CAD" = "CAS", #"Carassin_dore"<-"Carassin"
"BRG" = "BRE", #"Hybride_breme-gardon"<-"Breme" n=4
"HYC" = "GAR", #"Hybrides_de_cyprinides"<-"Gardon"n=20
"LPX" = "LPP", #"Lamproie"<-"Lamproie_de_planer" n=1047
"TRL" = "TRF", #"Truite_de_lac"<-"Truite_de_riviere"
"TRM" = "TRF"  #"Truite_de_mer"<-"Truite_de_riviere"
)

fish_length %<>%
  mutate(
    species = str_replace_all(species, sp_to_replace)
  )

##################
#  Obtain opcod  #
##################

myload(lot_id_opcod, dir = mypath("data-raw","fishing_op_build"))

fish_length %<>%
  left_join(lot_id_opcod) %>%
  rename(length = fish) %>%
  select(opcod, species, length)


devtools::use_data(fish_length, overwrite = TRUE)
rm(lot_id_opcod)

########################################
#  Extract nb species and individuals  #
########################################

op_sp_ind <- fish_length %>%
  group_by(opcod, species) %>%
  summarise(nind = n()) %>%
  group_by(opcod) %>%
  summarise(
    nb_sp  = n(),
    nb_ind = sum(nind)
    )
devtools::use_data(op_sp_ind, overwrite = TRUE)

################################################################################
#                                Operation data                                #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op.rda"))

op %>% filter(is.na(date))

#Ok
mysave(op, dir = mypath("data-raw"), overwrite = TRUE)

################################################################################
#                            Operation description                             #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_desc.rda"))
op_desc
# Look ok

mysave(op_desc, dir = mypath("data-raw"))
################################################################################
#                         Operation environmental data                         #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_env.rda"))
op_env
colnames(op_env)

op_env %>%
  filter(
  amplitude_thermique_air_station <= 0 |
  temperature_air_station <= 0 |
  precipitation_bassin_versant <= 0 |
  pente_ligne_eau < 0
  )
# Convert columns in english
col_replacement <- c(
"pente_ligne_eau" = "slope",
"section_mouillee" = "width",
"durete_totale" = "water_hardness",
"temp_max_moyenne_eau" = "water_avg_30d_max_temp",
"temp_air_bassin_versant" = "air_avg_temp_basin",
"precipitation_bassin_versant" = "avg_rainfall_basin",
"amplitude_thermique_air_station" = "air_thermal_amplitude",
"temperature_air_station" = "air_avg_temp",
"commentaire" = "comment"
)
colnames(op_env) %<>% str_replace_all(., col_replacement)

mysave(op_env, dir = mypath("data-raw"))

################################################################################
#                                   Station                                    #
################################################################################

load(mypath("data-raw", "fishing_op_build", "station.rda"))
library('sf')
station <- sfc_as_cols(station, names = c("lon", "lat"))
st_geometry(station) <- NULL

station %<>%
  rename(precise_location = localisation_precise) %>%
  select(id, precise_location, com_code_insee, lon, lat)

mysave(station, dir = mypath("data-raw"))
