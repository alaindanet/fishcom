################################################################################
#                        Explore naiades chemestry data                        #
################################################################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data <- read_csv2(mypath("data-raw", "naiades_temperatures", "analyse.csv"))
str(data[, "CdRqAna"])
unique(data[, "NomRdd"])
replacement_col <- c(
  "CdStationMesureEauxSurface" = "id",
  "LbSupport" = "substrate",
  "CdFractionAnalysee" = "id_portion",
  "LbFractionAnalysee" = "portion",
  "DatePrel" = "date",
  "CdParametre" = "id_parameter",
  "LbLongParamètre" = "parameter",
  "RsAna" = "value",
  "SymUniteMesure" = "units",
  "MnemoStatutAna" = "statut",
  "CdRqAna" = "id_rq",
  "LbQualAna" = "qualification"
)
#data %<>% .[, names(replacement_col)]
#colnames(data) %<>% str_replace_all(., replacement_col)


#nit <- filter(data, LbLongParamètre == "Nitrates") %>%
  #select(CdStationMesureEauxSurface, LbSupport, LbFractionAnalysee, DatePrel, LbLongParamètre, RsAna, LbQualAna) %>%
  #mutate(RsAna = as.numeric(RsAna)) %>%
  #arrange(CdStationMesureEauxSurface, DatePrel)

#min(nit$DatePrel)
#max(nit$DatePrel)
#summary(nit$RsAna)
#filter(nit, RsAna > 100)
#filter(nit, LbQualAna == "Correcte", RsAna > 100)

#myload(station_naiades, dir = mypath("data-raw"))
#bre_station <- filter(station_naiades, id) 

##########################
#  Transform csv in rda  #
##########################
