

#'  This file creates simulated on-farm experiments, and estimates site-specific
#'  yield response coefficients by GWR model.



## ==============================================================
##                          Preparation                        =
## ==============================================================

rm(list = ls())


# === Packages ===#
library(sp)
library(spdep)
library(spatialreg)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)
library(magic)
library(stringr)
library(ggplot2)
library(tictoc)
library(here)
options(stringsAsFactors = FALSE)

# === Set working directory ===#
setwd(here())

# === load functions ===#
#* source all the functions in the Functions folder
fs::dir_ls(here("GitControlled", "Codes", "Functions"), full.names = TRUE) %>%
  lapply(., function(x) source(x))

# /*===========================================================
#' # Load simulated regression data
# /*===========================================================
field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))

# /*===========================================================
#' # GWR estimation of MC simulation data
# /*===========================================================

#*************************************
# choose the kernel used for GWR from:
#   "gaussian", "bisquare", "exponential", "tricube", "boxcar"
#*************************************

kernel_choice <- "gaussian"
obw_choice <-
  ifelse(
    kernel_choice %in% c("gaussian", "exponential"),
    # continuous kernel
    18,
    # discontinuous kernel
    100
  )

mc_sim_results <-
  lapply(
    1:nrow(field_with_design),
    function(x) run_mc_sim(x, field_with_design)
  ) %>%
  rbindlist()

#* save the field parameters
saveRDS(mc_sim_results, here("Shared", "Results", kernel_choice, "mc_sim_results.rds"))
