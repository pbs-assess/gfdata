# Overview
# - Iterates over species and ssid to call both get_*survey_sets()
# - Stores extra sets relative to other function (x)
# - Stores unlike rows/repetitions for same comparison id (u)
# - Stores summary of returns (s)
# - Stores all sets when any differed (a)

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "functions-sets.R"))

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
# spp <- "009" # Rougheye

# Compare survey sets ----------------------------------------------------------

d <- compare_survey_sets(spp = spp, ssid = ssid)

# Extra sets
x <- d$x

# Unlike values
u <- d$u

# Summary of returns
s <- d$s

# All sets when any differed
a <- d$a

# Write results ----------------------------------------------------------------

saveRDS(x, file = "compare/results/sets-extras.rds")
saveRDS(u, file = "compare/results/sets-unlike.rds")
saveRDS(s, file = "compare/results/sets-summary.rds")
saveRDS(a, file = "compare/results/sets-alldiff.rds")
