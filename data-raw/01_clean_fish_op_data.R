################################################################################
#                       Clean fishing operation dataset                        #
################################################################################

# Adapted from the script of Willem Bonnafé:
# ../bonnafe_work/data/raw_data/cleanData.r

library(tidyverse)
library(magrittr)
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

devtools::use_data(fish_length)
rm(fish_length)

################################################################################
#                                Operation data                                #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op.rda"))

op %>% filter(is.na(date))
op %>%
  filter(temp_max_moyenne_eau <= 0) %>%
  select(temperature_air_station, temp_max_moyenne_eau, amplitude_thermique_air_station)
# A lot of 0 in temp_max_moyenne_eau, looks very strange
# 9720 op over 26800: 36% des op 
# I guess that those data were not measured:
op %<>% mutate(temp_max_moyenne_eau = ifelse(temp_max_moyenne_eau <= 0, NA, temp_max_moyenne_eau))

op %>%
  filter(
  surface_calculee <= 0
  ) %>%
  select(id, date, station, protocol, surface_calculee)
# Computed surface has not been filled
op %<>% mutate(surface_calculee = ifelse(surface_calculee <= 0, NA, surface_calculee))
op

op %>%
  filter(
  amplitude_thermique_air_station <= 0 |
  temperature_air_station <= 0 |
  precipitation_bassin_versant <= 0 |
  pente_ligne_eau < 0
  )
#Ok
save(op, file = mypath("data", "op.rda"))

################################################################################
#                            Operation description                             #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_desc.rda"))
op_desc
# Look ok
op_desc %<>%
  filter(ope_id %in% op$id)

save(mypath("data", "op_desc.rda"))
################################################################################
#                         Operation environmental data                         #
################################################################################

load(mypath("data-raw", "fishing_op_build", "op_env.rda"))
op_env
# Convert columns in english

################################################################################
#                                   Station                                    #
################################################################################

load(mypath("data-raw", "fishing_op_build", "station.rda"))
station %>%
  select(id, precise_location, x, y)
HERE

