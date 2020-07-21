# This script illustrates how to use multiple cores of a single cluster node#
# for a repeated calculation with the parallel package in R

library(parallel)

cat("Number of cores:")
detectCores()
cat("\n")

# Fast
f <- system.time({
  r <- mclapply(1:10, function(i)
    {
      Sys.sleep(10)  ## Do nothing for 10 seconds
    }, mc.cores = 5)
})

cat("Parallel computation time:\n")
f
cat("\n")

# slow

s <- system.time({
  r <- mclapply(1:10, function(i)
    {
      Sys.sleep(10)  ## Do nothing for 10 seconds
    }, mc.cores = 1)
})

cat("Serial computation time:\n")
s
cat("\n")
