# Contains Rstudio and the tydiverse
FROM rocker/verse:latest

# Custom packages
## Map
RUN R -e "install.packages(c('sf', 'rmapshaper'), repos = 'https://cloud.r-project.org/')"
## Network 
RUN R -e "install.packages(c('NetIndices', 'rnetcarto', 'igraph', 'betalink'), repos = 'https://cloud.r-project.org/')"
## Program
RUN R -e "install.packages(c('furrr'), repos = 'https://cloud.r-project.org/')"
