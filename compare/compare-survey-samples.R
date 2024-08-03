# Overview
# - Iterates over species and ssid to call both get_survey_samples*()
# - Identifies which calls led to error/not-error returns (e1 & e2)
# - Stores extra speciments relative to other function (s1 & s2)
# - Stores unlike rows/repetitions for same specimen id (cd)

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

# Compare specimens ------------------------------------------------------------

cs <- compare_specimens(spp = spp, ssid = ssid)

# Errored calls
e1 <- cs$e1 |> left_join(id_tab, by = "ssid")
# e1 |> view()
# e1 |> dplyr::filter(returned == "no")

e2 <- cs$e2 |> left_join(id_tab, by = "ssid")
# e2 |> view()
# e2 |> dplyr::filter(returned == "no")

# Extra specimens
s1 <- cs$s1
# s1 |> nrow()

s2 <- cs$s2
# s2 |> nrow()

# Write results
saveRDS(e1, file = "compare/results/errors-samples.rds")
saveRDS(e2, file = "compare/results/errors-samples2.rds")
saveRDS(s1, file = "compare/results/extras-samples.rds")
saveRDS(s2, file = "compare/results/extras-samples2.rds")


# Compare specimen values ------------------------------------------------------

cd <- compare_specimen_values(spp = spp, ssid = ssid)

# Rows that don't match identified by specimen_id and fn (d1 or d2)
cd

# Write results
saveRDS(cd, file = "compare/results/unlike-samples.rds")
