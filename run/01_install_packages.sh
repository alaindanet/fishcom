#!/bin/bash

./install.R tidyverse magrittr NetIndices rnetcarto igraph furrr devtools tictoc
./install.R lubridate roxygen2 betalink

# For the vignettes:
./install.R ggpmisc cowplot kableExtra tidygraph ggraph
# Misc:
./install.R default rprojroot tictoc

echo $R_LIBS_USER
echo "Packages installed!"
