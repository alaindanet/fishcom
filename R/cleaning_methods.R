#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

sp_to_remove <- function () {
  # Remove migratory species, species from lake, crayfish or very rare ones
  c(
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

}

sp_to_replace <- function () {
  c(
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
}
