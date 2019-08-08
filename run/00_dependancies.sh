#!/bin/bash

# Littler cannot be installed on the cluster :(
#R -e "if (!'littler' %in% installed.packages()[, 'Package']) \
#{install.packages(c('littler'), repos = 'https://cran.rstudio.com')} else \
#{cat('littler is already installed')}"
#wait
#echo "Littler installed!"

DLDIR="$HOME/downloads/"

# Pandoc (necessary to compile Rmarkdown)
pkgs='pandoc'
PANDOCURL="https://github.com/jgm/pandoc/releases/download/2.7.1/pandoc-2.7.1-linux.tar.gz"
PANDOCTAR="$DLDIR/pandoc-2.7.1-linux.tar.gz"
if ! dpkg -s $pkgs >/dev/null 2>&1; then
    echo "Pandoc is not installed"
    wget -P $DLDIR $PANDOCURL
    tar xvzf $PANDOCTAR --strip-components 1 -C $HOME/.local/ 
    echo "Pandoc is installed!"
    
fi

# Gdal
pkgs='gdal'
URL="http://download.osgeo.org/gdal/2.4.1/gdal-2.4.1.tar.gz"
TAR="$DLDIR/gdal-2.4.1.tar.gz"
if ! dpkg -s $pkgs >/dev/null 2>&1; then
    echo "$(pkgs) is not installed"
    wget -P $DLDIR $URL
    tar xvzf $TAR --strip-components 1 -C $HOME/.local/ 
    echo "$(pkgs) is installed!"
fi
