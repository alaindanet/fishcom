####################
#  Setup parallel  #
####################

library(future)

nb_cores <- future::availableCores()
#Crap hack to determine if in local or on the PBS cluster:
node_name <- Sys.info()["nodename"]
if (node_name %in% c("Andy", "BigAndy", "BrickAndy")) {
  future::plan(multiprocess)
#} else if (node_name %in% c("mesu0", "migale")) {
} else {
  cl <- parallel::makeCluster(nb_cores)
  future::plan(cluster, workers = cl)
  #warning("Unknow location: parallel session have not been set up")
}

