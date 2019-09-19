
mypath <- rprojroot::find_package_root_file
data_common <- mypath("data")
dest_dir <- mypath("data", "species")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(10, 10),
  fig.show = "hold",
  echo = FALSE,
  message = FALSE,
  warnings = FALSE
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
source(mypath("R", "misc.R"))
source(mypath("R", "plot_methods.R"))
theme_set(theme_alain())
