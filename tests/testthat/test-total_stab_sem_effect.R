mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "total_sem_effect.R"))

myload(stab_sem_rich, dir = mypath("tests"))

total_effect_stab_sem <- compute_stab_sem_indirect(sem = stab_sem_rich,
  p_val_thl = 0.05)

params <- get_coefficient_piecewisesem(sem = stab_sem_rich, p_val_thl = 0.05)
all_params <- get_coefficient_piecewisesem(sem = stab_sem_rich, p_val_thl = NULL)

test_that("conversion of sem object works", {
  expect_is(params, "data.frame")
  expect_equal(colnames(params), c("resp", "pred", "p_val", "std_est"))
  })

test_that("simple effect works", {
  debug(get_direct_effect_sem)
  stab_comp_effect_on_stab <- get_direct_effect_sem(
    fit = all_params,
    predictor = c("log_sync", "log_cv_sp"),
    response = "log_stab",
    p_val_thl = 0.05, output_names = "predictor")

  expected <- c(-1.2594, -0.7536)
  names(expected) <- c("log_cv_sp", "log_sync")
  expect_equal(stab_comp_effect_on_stab, expected)

  rich_effect_on_sync <- get_direct_effect_sem(
    fit = all_params,
    predictor = c("log_rich_tot"),
    response = "log_sync",
    p_val_thl = 0.05,
    output_names = "predictor"
  )

  expected <- c(-0.7627)
  names(expected) <- c("log_rich_tot")
  expect_equal(rich_effect_on_sync, expected)

  com_effect_on_stab_comp <- purrr::map(c(ct = "ct", t_lvl = "t_lvl"), 
    ~get_direct_effect_sem(
    fit = all_params,
    predictor = .x,
    response = c("log_sync", "log_cv_sp"),
    p_val_thl = 0.05,
    output_names = "response"
    )
  )


})

library(magrittr)
library(tidyverse)
#debug(get_indirect_effect)
get_indirect_effect(fit = all_params, p_val_thl = 0.05,
  x = c("ct", "t_lvl"),
  y = c("log_sync", "log_cv_sp"),
  z = "log_stab"
)

get_indirect_effect(fit = all_params, p_val_thl = 0.05,
  x = c("log_rich_tot"),
  z = c("log_sync", "log_cv_sp"),
  zz = c("log_stab") 
)

get_indirect_effect(fit = all_params, p_val_thl = 0.05,
  x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
  y = c("log_rich_tot"),
  z = c("ct", "t_lvl"),
  zz = c("log_sync", "log_cv_sp"),
  zzz = c("log_stab") 
)

indir_env_rich_stab <- get_indirect_effect(fit = all_params, p_val_thl = 0.05,
  x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
  y = c("log_rich_tot"),
  zz = c("log_sync", "log_cv_sp"),
  zzz = c("log_stab") 
)
indir_env_rich_stab %>%
  group_by(a) %>%
  summarise(indir = sum(indir))

indir_env_stab %>%
  group_by(a) %>%
  summarise(indir = sum(indir))

indir_env_stab <- get_indirect_effect(fit = all_params, p_val_thl = 0.05,
  x = c(paste0("log_RC", seq(1,3)), paste0("RC", c(4,5))),
  zz = c("log_sync", "log_cv_sp")
)
