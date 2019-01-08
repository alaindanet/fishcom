###################################################
#  Generation of fish size quantile distribution  #
###################################################
 
library('tidyverse')
library('magrittr')
devtools::load_all()


#########################
#  Cleaned data reload  #
#########################
data(fish_length)


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
## Not the same number of species:
all_equal(quant_willem, quant_to_compare)
## Indeed, we removed the migratory species as in bonnafé script: cleanData.r
all_equal(filter(quant_willem, !(species %in% c("SAT", "LPP", "LPR"))),
  quant_to_compare)
## Looks ok! 

##########
#  Save  #
##########
fish_size_quantile <- quant_to_compare
devtools::use_data(fish_size_quantile)
