##############################
#  truta example for figure  #
##############################

library(tidyverse)
library(magrittr)
library(cowplot)
theme_set(theme_cowplot())

mypath <- rprojroot::find_package_root_file
source(mypath("R", "plot_methods.R"))
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

myload(length_analysis, dir = mypath("data"))

truta_length <- length_analysis %>%  
  filter(species == "TRF")

summary(truta_length)

p_truta_length <- truta_length %>%
  ggplot(aes(x = length)) +
  geom_histogram(binwidth = 10)

myload(metaweb_analysis, dir = mypath("data"))
names(metaweb_analysis)

truta_size_class <- metaweb_analysis$size_class %>%
  filter(species == "TRF")
size_class_segment <- c(0, truta_size_class$upper) 

size_class_segment_df <- data.frame(y = -1000, length = size_class_segment)

p <- p_truta_length + 
  scale_shape_identity() +
  geom_point(
    data = size_class_segment_df,
    aes(y = y, x = length, shape = 124, size = 5)) +
  geom_line(data = size_class_segment_df, aes(y = y, x = length, size = 1)) +
  labs(x = "Body size (mm)", y = "Frequency") +
  theme(legend.position = "none")

save_plot(
  filename = mypath("manuscript", "bef_stability", "figs", "truta_body_size.pdf"),
  plot = p#,
  #base_height = ,
  #base_width = 
)

# Fish ontogenic size
myload(diet_shift, dir = mypath("data"))
truta_diet_shift <- diet_shift %>% 
  filter(species == "TRF")

truta_diet_shift %>%
  select(light:fish)
