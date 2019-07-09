################################################################################
#                        Explore naiades chemestry data                        #
################################################################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data <- read_csv2(mypath("data-raw", "polluants", "naiades_data", "analyses_2005.csv"))
str(data[, "CdRqAna"])
unique(data[, "NomRdd"])
replacement_col <- c(
  "CdStationMesureEauxSurface" = "id",
  "LbSupport" = "substrate",
  "LbFractionAnalysee" = "portion",
  "DatePrel" = "date",
  "HeurePrel" = "time",
  "CdParametre" = "id_parameter",
  "LbLongParamètre" = "parameter",
  "RsAna" = "value",
  "SymUniteMesure" = "units",
  "MnemoStatutAna" = "statut",
  "LbQualAna" = "qualification"
)
data %<>% .[, names(replacement_col)]
colnames(data) %<>% str_replace_all(., replacement_col)
analysis_files <- list.files(
  path = mypath("data-raw", "polluants", "naiades_data"),
  pattern = "analyses*", full.names = FALSE)

sapply(analysis_files, function (file_name) {
  temp <- read_csv2(mypath("data-raw", "polluants", "naiades_data", file_name))
  temp %<>% .[, names(replacement_col)]
  colnames(temp) %<>% str_replace_all(., replacement_col)
  obj <- str_sub(file_name, end = -5)
  assign(obj, temp, envir = .GlobalEnv)
  save(list = obj,
    file = mypath("data-raw", "polluants", "naiades_data", paste0(obj, ".rda")),
    compress = "bzip2"
  )
  })


nit <- filter(data, LbLongParamètre == "Nitrates") %>%
  select(CdStationMesureEauxSurface, LbSupport, LbFractionAnalysee, DatePrel, LbLongParamètre, RsAna, LbQualAna) %>%
  mutate(RsAna = as.numeric(RsAna)) %>%
  arrange(CdStationMesureEauxSurface, DatePrel)

min(nit$DatePrel)
max(nit$DatePrel)
summary(nit$RsAna)
filter(nit, RsAna > 100)
filter(nit, LbQualAna == "Correcte", RsAna > 100)

myload(station_naiades, dir = mypath("data-raw"))
bre_station <- filter(station_naiades, id) 

