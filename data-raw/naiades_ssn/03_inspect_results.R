library(tidyverse)
library(magrittr)
library(sf)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "geo_methods.R"))
source(mypath("R", "plot_methods.R"))

basin <- c("ouest", "sud", "est", "nord")

# Load dataset:
data_list <- lapply(basin, function (x) {
  obj <- paste0(x, "_interpolation")
  load(mypath("data-raw", "polluants", paste0(obj, ".rda")))
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
  
yearly_press_interp <- interpolation %>%
  unnest(prediction) %>%
  select(id, year, parameter, value, value.predSE)

cv_press_interp <- interpolation %>%
  unnest(cross_v) %>%
  select(-ssn, -prediction)

mysave(yearly_press_interp, cv_press_interp,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)

myload(yearly_press_interp, dir = mypath("data-raw", "polluants"))

# Filter param that have a lot of interpolation fails
eff_interp <- yearly_press_interp %>%
  group_by(parameter) %>%
  summarise(frac_obs = sum(!is.na(value)) / n())
yearly_press_interp %<>%
  filter(!parameter %in% unique(eff_interp[which(eff_interp$frac_obs < .80), ]$parameter))

myload(dist_yearly_avg_polluants, dir = mypath("data-raw", "polluants"))

# Filter abberant values:
options(mc.cores = 30)
library(parallel)
yearly_press_interp$value_corrected <- mcMap(
  function (param, x, distri) {
    mask <- which(distri$parameter == param)
    check <- distri[mask,!names(distri) %in% "parameter"] %>%
      unlist
    extrem_check <- x > check["max"] | x < check["min"]
    if(extrem_check | is.na(x)) {
      return(NA)
    } else {
      return(x)
    }
  },
  param = yearly_press_interp$parameter,
  x = yearly_press_interp$value,
  MoreArgs = list(
    distri = dist_yearly_avg_polluants 
  )
  )
yearly_press_interp$value_corrected <- 
  sapply(yearly_press_interp$value_corrected, function(x) x[1])

mysave(yearly_press_interp,
  dir = mypath("data-raw", "polluants"), overwrite = TRUE)
