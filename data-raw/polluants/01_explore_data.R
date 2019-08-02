################################################################################
#                        Explore naiades chemestry data                        #
################################################################################

library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

data2 <- read_csv2(mypath("data-raw", "polluants", "naiades_data", "analyses_2016.csv"))
data2 <- data.table::fread(mypath("data-raw", "polluants", "naiades_data",
    "analyses_2016.csv"))
data2 %<>% as_tibble()
str(data2[, "CdRqAna"])
unique(data2[, "NomRdd"])
replacement_col <- c(
  "CdStationMesureEauxSurface" = "id",
  "LbSupport" = "substrate",
  "CdFractionAnalysee" = "id_portion",
  "LbFractionAnalysee" = "portion",
  "DatePrel" = "date",
  "HeurePrel" = "time",
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

################
#  Operations  #
################
data2 <- read_delim(
  mypath("data-raw", "polluants", "naiades_data", "operations_2018.csv"),
  delim = ";", locale = locale(decimal_mark = ","),
  col_types = cat(c(rep("?", 26), rep("-", 6)))
)

analysis_files <- list.files(
  path = mypath("data-raw", "polluants", "naiades_data"),
  pattern = "operations.*.csv", full.names = FALSE)

options(mc.cores = 8)
parallel::mclapply(analysis_files, function (file_name) {
  temp <- read_csv2(mypath("data-raw", "polluants", "naiades_data", file_name),
  col_types = cat(c(rep("?", 26), rep("-", 6)))
  )

  obj <- str_sub(file_name, end = -5)
  assign(obj, temp, envir = .GlobalEnv)
  save(list = obj,
    file = mypath("data-raw", "polluants", "naiades_data", paste0(obj, ".rda")),
    compress = "bzip2"
  )
  invisible()
  })

operation_files <- list.files(
  path = mypath("data-raw", "polluants", "naiades_data"),
  pattern = "operations.*.rda", full.names = FALSE)

test <- parallel::mclapply(operation_files, function (files) {

  obj_name <- str_sub(files, end = -5) 
  print(obj_name)
  load(mypath("data-raw", "polluants", "naiades_data", files))
  data <- get(obj_name)
  data
  })
data <- do.call(rbind, test)

########################
#  Get press category  #
########################
press_cat <- read_csv2(mypath("data-raw", "polluants", "naiades_data",
    "pressure_info_olivier.csv")) %>%
  rename(parameter = lblong_param) %>%
  select(parameter, direction, category)

replace_rules <- c(
  "\\(" = "-",
  "\\)" = "-",
  "\\'" = ""
)
press_cat %<>%
  mutate(
    parameter = stringi::stri_trans_general(parameter, "Latin-ASCII"),
    parameter = tolower(parameter),
    parameter = str_replace_all(parameter, " ", "_"),
    parameter = str_replace_all(parameter, replace_rules),
    category = tolower(category)
    )
sup_category <- c(
  "tetrachloroethylene" = "industry", #increasing
  "dbo5" = "matieres organiques",# increasing 
  "tributyletain_oxyde" = "fungicides",# increasing
  "durete_totale" = "other", #increasing
  "tetrachl.carbone" = "industry", #increasing
  "dibutyletain_dichlorure" = "industry",
  "dibutyletain_oxyde" = "industry",
  "octabromodiph_ether_205" = "industry",
  "octabromodiph_ether_203" = "industry"
)
supp_press_cat <- tibble(
  parameter = names(sup_category),
  direction = "increasing",
  category = sup_category
)
press_cat <- rbind(press_cat, supp_press_cat) %>%
  filter(!is.na(category))
mysave(press_cat, dir = mypath("data-raw", "polluants"), overwrite = TRUE)

# To make the database:
press_cat %<>%
  mutate(ld50 = NA, adi = NA, arfd = NA, aoel = NA)
#write_csv(press_cat, path = mypath("data-raw", "polluants", "polluants_effects.csv"))

# To get the database in rda:
press_cat <- read_delim(mypath("data-raw", "polluants", "polluants_effects.csv"), delim = ";", locale = locale(decimal_mark = ","))
mysave(press_cat, dir = mypath("data-raw", "polluants"), overwrite = TRUE)
