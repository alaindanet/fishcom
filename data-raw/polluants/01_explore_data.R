################################################################################
#                        Explore naiades chemestry data                        #
################################################################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data <- read_csv2(mypath("data-raw", "polluants", "naiades_data", "analyses_2016.csv"))
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

analysis_files <- list.files(
  path = mypath("data-raw", "polluants", "naiades_data"),
  pattern = "analyses.*.csv", full.names = FALSE)
pressure_info <- read_csv2(mypath("data-raw", "polluants", "naiades_data", "pressure_info_olivier.csv")) 

library(parallel)
options(mc.cores = 8)
mclapply(analysis_files, function (file_name) {
  if (file_name == "analyses_2016.csv") { # bug with integer64 in col CdPreleveur
    temp <- data.table::fread(mypath("data-raw", "polluants", "naiades_data",
    "analyses_2016.csv"))
    temp %<>% as_tibble()
  } else {
    temp <- read_csv2(mypath("data-raw", "polluants", "naiades_data", file_name))
  }

  temp %<>% .[, names(replacement_col)]
  colnames(temp) %<>% str_replace_all(., replacement_col)
  temp %<>%
    filter(id_parameter %in% unique(pressure_info$cd_param)) %>%
    filter(!is.na(date)) %>% #enlever les données sans date
    filter(!value<0) %>% #enlever les valeurs négatives = valeurs minimales aberrantes
    filter(id_portion %in% c("23","3")) %>% #sélectionner les fractions d'intérêt
    filter(qualification == "Correcte") %>% #sélectionner les données "correctes"
    filter(!id_rq %in% c(0, 8, 9))# supprimer les analyses non faites et microbio

  obj <- str_sub(file_name, end = -5)
  assign(obj, temp, envir = .GlobalEnv)
  save(list = obj,
    file = mypath("data-raw", "polluants", "naiades_data", paste0(obj, ".rda")),
    compress = "bzip2"
  )
  invisible()
  })

myload(analyses_2016, dir = mypath("data-raw", "polluants", "naiades_data"))

