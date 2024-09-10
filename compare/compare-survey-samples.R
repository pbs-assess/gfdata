# Overview
# - Iterates over species and ssid to call both get_*survey_samples()
# - Stores extra speciments relative to other function (x)
# - Stores unlike rows/repetitions for same specimen id (u)
# - Stores summary of returns (s)
# - Stores all specimens when any differed (a)

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "functions-samples.R"))

# Get ssids --------------------------------------------------------------------
id_tab <- readRDS("compare/data/ssids.rds")
ssid <- id_tab |> dplyr::pull(ssid)

# Get species ------------------------------------------------------------------

spp <- readRDS("compare/data/species.rds")[c(1:3, 5:12)]
# [c(1, 5, 6, 8, 10)]

# Print
cat("spp = \n", paste0(spp, "\n"))

# Shorter species set
# spp <- "225" # Hake
# spp <- "044" # Dogfish
# spp <- "009" # Rougheye (Not complex) (Note: errors in get_survey_samples())

# Compare survey samples -------------------------------------------------------

d <- compare_survey_samples(spp = spp, ssid = ssid, areas = NULL)

# Extra specimens
x <- d$x

# Unlike values
u <- d$u

# Summary of returns
s <- d$s

# All specimens when any differed
a <- d$a

# Write results ----------------------------------------------------------------

saveRDS(x, file = "compare/results/samples-extras.rds")
saveRDS(u, file = "compare/results/samples-unlike.rds")
saveRDS(s, file = "compare/results/samples-summary.rds")
saveRDS(a, file = "compare/results/samples-alldiff.rds")
