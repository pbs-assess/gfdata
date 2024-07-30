# Overview
# - Iterates over species and ssid to call both get_survey_sets*()
# - Identifies which calls led to error/not-error returns (e1 & e2)
# - Stores extra fishing events relative to other function (s1 & s2)
# - Stores unlike rows/repetitions for same fishing event id (cd)

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "functions-sets.R"))

# Get ssids --------------------------------------------------------------------
id_tab <- readRDS("compare/data/ssids.rds")
ssid <- id_tab |> dplyr::pull(ssid)

# Get species ------------------------------------------------------------------

spp <- readRDS("compare/data/species-names.rds")

# Shorter species set
# spp <- "225" # Hake
# spp <- "044" # Dogfish
# spp <- "009" # Rougheye

# Compare sets -----------------------------------------------------------------

cs <- compare_sets(spp = spp, ssid = ssid)

# Errored calls
e1 <- cs$e1 |> left_join(id_tab, by = "ssid")
e1 |> view()
e1 |> dplyr::filter(returned == "no")

e2 <- cs$e2 |> left_join(id_tab, by = "ssid")
e2 |> view()
e2 |> dplyr::filter(returned == "no")

# Extra sets
s1 <- cs$s1
s1 |> nrow()

s2 <- cs$s2
s2 |> nrow()

# Write results
saveRDS(e1, file = "compare/data/errors-sets.rds")
saveRDS(e2, file = "compare/data/errors-sets2.rds")
saveRDS(s1, file = "compare/data/extras-sets.rds")
saveRDS(s2, file = "compare/data/extras-sets2.rds")

# Compare set values ------------------------------------------------------

cd <- compare_set_values(spp = spp, ssid = ssid)

# Rows that don't match identified by fishing_event_id and fn (d1 or d2)
cd

# Write results
saveRDS(cd, file = "compare/data/unlike-sets.rds")
