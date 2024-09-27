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

spp <- readRDS("compare/data/species.rds")

# Print
cat("spp = \n", paste0(spp, "\n"))

# Compare survey samples -------------------------------------------------------

# Notes
#
# For get_all_survey_samples():
# - Arguments default to comparison values
#
d <- compare_survey_samples(spp = spp,
                            ssid = ssid,
                            # Args for get_survey_samples()
                            get_arg_remove_bad_data = TRUE,
                            get_arg_unsorted_only = TRUE,
                            get_arg_usability = NULL,
                            get_arg_major = NULL,
                            # Args for get_all_survey_samples()
                            get_all_arg_major = NULL,
                            get_all_arg_usability = NULL,
                            get_all_arg_unsorted_only = TRUE, # Check
                            get_all_arg_random_only = TRUE, # Check
                            get_all_arg_grouping_only = TRUE, # Check
                            get_all_arg_include_event_info = TRUE, # Check
                            get_all_arg_include_activity_matches = FALSE,
                            get_all_arg_remove_bad_data = TRUE,
                            get_all_arg_remove_duplicates = TRUE,
                            get_all_arg_return_dna_info = FALSE,
                            get_all_arg_drop_na_columns = TRUE,
                            get_all_arg_quiet_option = "message")

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
