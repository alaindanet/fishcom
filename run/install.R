#!/usr/bin/env Rscript 

argv <- commandArgs(trailingOnly=TRUE)

if (is.null(argv) | length(argv) < 1) {
  cat("Usage: installr.r pkg1 [pkg2 pkg3 ...]\n")
  q()
}

## adjust as necessary, see help('download.packages')
repos <- "https://cran.rstudio.com"

## this makes sense on Debian where no packages touch /usr/local
lib.loc <- "~/.local/share/R-3.6.1/library/"

## Remove from the list already installed packages
already_installed <- installed.packages()[, "Package"]
argv <- argv[which(!argv %in% already_installed)]

if (is.null(argv) | length(argv) < 1) {
  cat("All the packages were already installed\n")
} else {
  install.packages(argv, lib.loc, repos)
  cat("The following packages have been installed:\n", argv, ".\n")
}



