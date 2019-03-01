#!/bin/bash

R -e "if (!'littler' %in% installed.packages()[, 'Package']) \
{install.packages(c('littler'), repos = 'https://cloud.r-project.org/')} else \
{cat('littler is already installed')}"

wait

echo "Littler installed!"
