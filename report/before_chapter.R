
mypath <- rprojroot::find_package_root_file
data_common <- mypath("data")
dest_dir <- mypath("data", "species")

knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(7, 7),
  fig.show = "hold",
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)

library(MASS)
library(tidyverse)
library(magrittr)
library(ggpmisc)
library(cowplot)
library(kableExtra)
library(sf)
library(ggraph)
library(lubridate)
library(pander)
library(broom)
source(mypath("R", "misc.R"))
source(mypath("R", "plot_methods.R"))
source(mypath("R", "geo_methods.R"))
theme_set(theme_alain())
