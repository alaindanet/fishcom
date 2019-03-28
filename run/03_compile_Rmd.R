#!/usr/bin/env Rscript
options(echo = TRUE)

Sys.getenv("RSTUDIO_PANDOC")

cat("Compile results\n")
rmarkdown::render("../vignettes/results.Rmd")
