#!/bin/bash

R -e "if (!'littler' %in% installed.packages()[, 'Package']) \
{install.packages(c('littler'), repos = 'https://cran.rstudio.com')} else \
{cat('littler is already installed')}"

wait

echo "Littler installed!"
