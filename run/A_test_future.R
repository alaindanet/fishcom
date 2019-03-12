#!/usr/bin/env Rscript
# Args should be to specify the type of cluster you are on or at home in order
# to find the cpus
# See: ?availableCores
args = commandArgs(trailingOnly=TRUE)
if (!is.null(args) & length(args) > 1) {
  stop("Only one argument should be specified")
} else if (!is.null(args) & is.character(args)) {
  options(future.availableCores.methods = args[1])
}
options(echo = TRUE)

library(future)

nb_cores <- availableCores()
print(nb_cores)
cores <- paste("n", seq(1, nb_cores))

cl <- parallel::makeCluster(nb_cores)
plan(cluster, workers = cl)

pid <- Sys.getpid()
pid
a %<-% {
    pid <- Sys.getpid()
    cat("Future 'a' ...\n")
    3.14
}
b %<-% {
    rm(pid)
    cat("Future 'b' ...\n")
    Sys.getpid()
}
c %<-% {
    cat("Future 'c' ...\n")
    2 * a
}
b
c
a

parallel::stopCluster(cl)
cat("Good!")
