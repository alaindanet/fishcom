#!/bin/bash

./install.R tidyverse magrittr NetIndices rnetcarto igraph furrr devtools tictoc
./install.R lubridate roxygen2

echo $R_LIBS_USER
echo "Packages installed!"
