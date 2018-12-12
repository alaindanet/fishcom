###################################################
#  Generation of fish size quantile distribution  #
###################################################
 
library('tidyverse')
library('magrittr')
devtools::load_all()

###############
#  Load data  #
###############
diet_shift <- read_delim("ontogenic_diet_shift_fish.csv",
  delim = ";", locale = locale(decimal_mark = "."))
pred_win <- read_delim("fish_fish_predation_window.csv",
  delim = ";", locale = locale(decimal_mark = "."))
data(fish_length)

###########################
#  Pre-data manipulation  #
###########################
colnames(pred_win) <- str_replace_all(colnames(pred_win),
  c("par_" = "", "1" = "alpha", "2" = "beta", "_code" = ""))
devtools::use_data(pred_win)

colnames(diet_shift) <- str_replace_all(colnames(diet_shift),
  c("species" = "species_name", "species_name_code" = "species"))
devtools::use_data(diet_shift)

#########################
#  Quantile estimation  #
#########################
quartile <- fish_length %>%
  group_by(species) %>%
  nest() %>%
  mutate(
    quartile = map(data, ~ quantile(.$length)),
    quartile = map(quartile, ~ bind_rows(.) %>% 
      gather(quartile, size))
    ) %>%
  unnest(quartile)

## Check
quant_willem <- read_delim("../bonnafe_work/data/bib_data/size_quantiles.csv", delim = ";", locale = locale(decimal_mark = "."))
colnames(quant_willem)[1] <- "species"
quant_willem %<>% arrange(species)
quant_to_compare <- spread(quartile, quartile, size) %>%
  select(species, `0%`, `25%`, `50%`, `75%`, `100%`) %>%
  arrange(species)
all_equal(quant_willem, quant_to_compare)

filter(quant_willem, row_number() %in% c(43, 33, 32))
filter(quant_to_compare, row_number() %in% c(43, 33, 32))
## Some fish have disappeared ? Because my ranges are always thiner
