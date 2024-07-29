# Overview
# - Script iterates over species and ssids
# - Compares specimen_id column for extras in each data frame
# - Identifies rows indexes for extra specimen_id values
# - Accumulates extra specimen_id rows for each iteration
#
# Wish list
# - Update get_survey_samples*() functions to avoid errors when no data
# - E.g. spp <- "009"; ssid <- 1; returns errors for both functions

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "functions-samples.R"))

# Get ssids --------------------------------------------------------------------
id_tab <- readRDS("compare/data/ssids.rds")
ssid <- id_tab |> dplyr::pull(ssid)

# Get species ------------------------------------------------------------------

spp <- readRDS("compare/data/species-names.rds")

# Shorter species set
# spp <- "225" # Hake
# spp <- "044" # Dogfish
# spp <- "009" # Rougheye

# Compare specimens ------------------------------------------------------------

cs <- compare_specimens(spp = spp, ssid = ssid)

# Errored calls
e1 <- cs$e1 |> left_join(id_tab, by = "ssid")
e1 |> view()
e1 |> dplyr::filter(returned == "no")

e2 <- cs$e2 |> left_join(id_tab, by = "ssid")
e2 |> view()
e2 |> dplyr::filter(returned == "no")

# Extra specimens
s1 <- cs$s1
s1 |> nrow()

s2 <- cs$s2
s2 |> nrow()

# Write results
saveRDS(e1, file = "compare/data/errors-samples.rds")
saveRDS(e2, file = "compare/data/errors-samples2.rds")
saveRDS(s1, file = "compare/data/extras-samples.rds")
saveRDS(s2, file = "compare/data/extras-samples2.rds")


# Compare columns --------------------------------------------------------------

cd <- compare_specimen_values(spp = spp, ssid = ssid)

# Rows that don't match identified by specimen_id and fn (d1 or d2)
cd

# Write results
saveRDS(cd, file = "compare/data/unlike-samples.rds")
