library("devtools")
options(devtools.name = "Alain Danet",
  devtools.desc.author = "person('Alain', 'Danet',
  email='alain.danet@caramail.fr', role = c('aut', 'cre'))",
  devtools.desc.license = "MIT + file LICENSE"
)

setup()
use_testthat()
use_vignette("intro")
use_travis()
use_package_doc()
use_cran_comments()
use_readme_rmd()
