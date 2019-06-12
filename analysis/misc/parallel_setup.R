####################
#  Setup parallel  #
####################


nb_cores <- future::availableCores()
cl <- parallel::makeCluster(nb_cores)
#Crap hack to determine if in local or on the PBS cluster:
node_name <- Sys.info()["nodename"]
if (node_name %in% c("Andy", "BigAndy")) {
  future::plan(multisession)
#} else if (node_name %in% c("mesu0", "migale")) {
} else {
  future::plan(cluster, workers = cl)
  #warning("Unknow location: parallel session have not been set up")
}

