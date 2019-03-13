####################
#  Setup parallel  #
####################


library(future)

nb_cores <- availableCores()
cl <- parallel::makeCluster(nb_cores)
#Crap hack to determine if in local or on the PBS cluster:
if (dir.exists("~/Documents")) {
  plan(multisession)
} else {
  plan(cluster, workers = cl)
}

