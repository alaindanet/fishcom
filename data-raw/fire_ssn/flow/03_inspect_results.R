library(tidyverse)
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))

basin <- c("ouest", "sud", "est", "nord")

# Load dataset:
data_list <- lapply(basin, function (x) {
  obj <- paste0(x, "_interp_mv_avg")
  load(mypath("data-raw", "flow", paste0(obj, ".rda")))
  assign("data", get(obj))
  return(data)
})
# Merge them:
interpolation <- do.call(rbind, data_list)

fill_empty_cross_v <- names(interpolation$ssn[[1]][["cross_v"]])
fill_empty_pred <- names(interpolation$ssn[[1]][["prediction"]])
id_by_basin <- interpolation %>%
  group_by(basin) %>%
  slice(1) %>%
  mutate(id = map(ssn, function(x) x[["prediction"]][, "id"])) %>%
  select(basin, id)
# Get pred and data.frame

interpolation %<>%
  mutate(
    cross_v = map(ssn, function(x) {
  if(is.null(names(x))) {
    out <- data.frame(
      matrix(NA, nrow = 1, ncol = length(fill_empty_cross_v))
    )
    colnames(out) <- fill_empty_cross_v
    out
  } else {
    x[["cross_v"]]
  }
}),
    prediction = map2(ssn, basin, function(x, basin) {
  if(is.null(names(x))) {
    ids <- id_by_basin[which(id_by_basin$basin == basin), ]$id
    out <- data.frame(
      matrix(NA, nrow = length(ids), ncol = 3)
    )
    out[, 1] <- ids
    colnames(out) <- fill_empty_pred
    out
  } else {
    x[["prediction"]]
  }
})
  )
  
yearly_flow_press_interp_mv_avg <- interpolation %>%
  unnest(prediction) %>%
  select(id, year, value, value.predSE)

cv_flow_press_interp_mv_avg <- interpolation %>%
  unnest(cross_v) %>%
  select(-ssn, -prediction)

mysave(yearly_flow_press_interp_mv_avg, cv_flow_press_interp_mv_avg,
  dir = mypath("data-raw", "flow"), overwrite = TRUE)

myload(yearly_flow_press_interp_mv_avg, dir = mypath("data-raw", "flow"))

# Filter param that have a lot of interpolation fails
eff_interp <- yearly_flow_press_interp_mv_avg %>%
  summarise(frac_obs = sum(!is.na(value)) / n())

myload(dist_yearly_avg_flow, dir = mypath("data-raw", "flow"))

# Filter abberant values:
options(mc.cores = 15)
library(parallel)
yearly_flow_press_interp_mv_avg$value_corrected <- mcMap(
  function (x, distri) {
    check <- distri %>%
      unlist
    if (is.na(x)) {
      return(NA)
    } else if (x < check["min"]) {
      return(check["min"])
    } else if (x > check["max"]) {
      return(check["max"])
    } else {
      return(x)
    }
  },
  x = yearly_flow_press_interp_mv_avg$value,
  MoreArgs = list(
    distri = dist_yearly_avg_flow 
  )
)
yearly_flow_press_interp_mv_avg$value_corrected <- 
  sapply(yearly_flow_press_interp_mv_avg$value_corrected, function(x) x[1])

# If several times interpolated flow:
# Keep the best prediction (lowest predSE):
yearly_flow_press_interp_mv_avg %<>%
  group_by(id, year) %>%
  arrange(value.predSE) %>%
  slice(1)


press_flow <- yearly_flow_press_interp_mv_avg %>%
  group_by(id) %>%
  summarise(
    flow = mean(value_corrected, na.rm = TRUE),
    cv_flow = sd(value_corrected, na.rm = TRUE) / flow
  )
filter(yearly_flow_press_interp_mv_avg, id == 10153) %>%
  arrange(year)

mysave(yearly_flow_press_interp_mv_avg, press_flow,
  dir = mypath("data-raw", "flow"), overwrite = TRUE)


