<!-- README.md is generated from README.Rmd. Please edit that file -->
fishcom
=======

[![Build
Status](https://travis-ci.org/alaindanet/fishcom.svg?branch=master)](https://travis-ci.org/alaindanet/fishcom)

The goal of fishcom is to get all the informations concerning the
ECOSTAB postdoc project in one place including reproducible code, notes,
eventually data and manuscripts.

How to
------

\#\#\# Get the repository

You can clone the repository with [git](https://git-scm.com/) by pasting
in your terminal:

    git clone https://github.com/alaindanet/fishcom.git dir_name_to_be_created

OR

just download the repository:
[alaindanet/fishcom](https://github.com/alaindanet/fishcom/archive/master.zip)

### Open it in Rstudio

If you have [Rstudio](https://www.rstudio.com/) installed on your
computer, just open `fishcom.Rproj` with Rstudio.

Information
-----------

My notes are available in the introductory vignette
`vignettes/intro.html`

Structure of the project repository
-----------------------------------

### Raw data

Raw datas are spreadsheets (xls, csv, etc…) shapefiles and others
cartographic data and reports (of the AFB, ONEMA, etc). The raw datas
come together with `R` scripts which describe cleaning procedures. The
cleaning procedures end by saving datasets used for data analysis.

It is located in `data-raw`.

### Data for analysis

The clean datasets live in `data` directory and stored in `rda`
compressed format. Once the package is loaded, they can be call as
easily as `load(objet_name)`.

``` r
devtools::load_all()
#> Loading fishcom
data(metaweb_analysis)
names(metaweb_analysis)
#> [1] "metaweb"         "species"         "resource"        "nb_class"       
#> [5] "size_class"      "piscivory_index" "th_prey_size"
```

The cleaned and filtered objects used for data analysis are named with
the suffix `_analysis`.

### Analysis of data

The analysis of data lives in the directory named `analysis`.

This folder contains scripts in which various metrics are computed for
two different aspects that have been more or less arbitrary separated.

#### Community data

The community data lives in the `community_metrics` and contains the
following metrics:

-   species abondances
-   species biomass
-   species richness

The following metrics are summarised by station:

-   mean and cv of species richness
-   mean and cv of species biomass
-   temporal beta-diversity

#### Network data

The network data lives in the `network_analysis` and contains the
following metrics:

-   connectance
-   number of nodes
-   modularity and modules
-   nestedness
-   trophic length

The following metrics are summarised by station:

-   mean and cv of all the above indices
