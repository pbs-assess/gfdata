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

spp <- readRDS("compare/data/species-sets.rds")

# Print
cat("spp = \n", paste0(spp, "\n"))

# Compare survey sets ----------------------------------------------------------

# Notes
#
# For get_survey_sets():
# - Drops all rows if all counts and weights each either zero or NA
#
# For get_all_survey_sets():
# - Arguments default to comparison values
#
d <- compare_survey_sets(spp = spp,
                         ssid = ssid,
                         # Args for get_survey_sets()
                         get_arg_join_sample_ids = FALSE,
                         get_arg_verbose = FALSE,
                         get_arg_remove_false_zeros = FALSE,
                         get_arg_sleep = 0.05,
                         # Args for get_all_survey_sets()
                         get_all_arg_major = NULL,
                         get_all_arg_years = NULL,
                         get_all_arg_join_sample_ids = FALSE,
                         get_all_arg_remove_false_zeros = FALSE, # Check
                         get_all_arg_remove_bad_data = TRUE,
                         get_all_arg_remove_duplicates = TRUE, # Check
                         get_all_arg_include_activity_matches = FALSE,
                         get_all_arg_usability = c(0, 1, 2, 6), # Check
                         get_all_arg_grouping_only = TRUE, # Check
                         get_all_arg_drop_na_columns = TRUE,
                         get_all_arg_quiet_option = "message")

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
