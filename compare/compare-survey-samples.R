# Overview
# - Iterates over species and ssid to call both get_survey_samples*()
# - Stores extra speciments relative to other function (x)
# - Stores unlike rows/repetitions for same specimen id (u)
# - Stores summary of returns (s)

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "functions-samples.R"))

# Get ssids --------------------------------------------------------------------
id_tab <- readRDS("compare/data/ssids.rds")
ssid <- id_tab |> dplyr::pull(ssid)

# Get species ------------------------------------------------------------------

spp <- readRDS("compare/data/species.rds")

# Shorter species set
# spp <- "225" # Hake
# spp <- "044" # Dogfish
# spp <- "009" # Rougheye (Not complex) (Note: errors in get_survey_samples())

# Compare survey samples -------------------------------------------------------

d <- compare_survey_samples(spp = spp, ssid = ssid, areas = areas)

# Extra specimens
x <- d$x

# Unlike values
u <- d$u

# Summary of returns
s <- d$s

# Write results ----------------------------------------------------------------

saveRDS(x, file = "compare/results/samples-extras.rds")
saveRDS(u, file = "compare/results/samples-unlike.rds")
saveRDS(s, file = "compare/results/samples-summary.rds")
