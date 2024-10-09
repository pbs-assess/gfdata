# Load
library(gfdata)
library(tidyverse)

# Source functions
source(here::here("inst", "compare", "compare-survey-sets.R"))
source(here::here("inst", "compare", "compare-survey-samples.R"))

# Define species and ssids
spp <- c("Yelloweye Rockfish", "Eulachon")

# ssids <- NULL
ssids <-  c(1,2,7,14,22) # shortlist spanning common types of surveys
# ssids <- c(1,2,3,4,6,7,14,16,22,36,39,40) # full list of gfsynopsis surveys
# ssids <- c(35, 68, 76, 82:87) # surveys excluded from gfsynopsis sablefish, hake, dogfish and jig

# Compare survey sets
de <- compare_survey_sets(spp = spp,
                         ssids = ssids, # NULL here (default) returns default ssids for og fn
                         ## settings for get_all function that match more closely behaviour of og fn
                         # usability = c(0, 1, 2, 6), # not default
                         # grouping_only = TRUE, # not default
                         remove_false_zeros = FALSE # not default
                         )

# Extra events for a given species-survey combination
de$x

# Unlike values
de$u

# Summary of returns
de$s

# All events when any differed
de$a


# Compare survey samples
ds <- compare_survey_samples(spp = spp,
                            ssids = ssids, # default is NULL for both
                            # major = NULL, # default is NULL for both
                            usability = c(0, 1, 2, 6), # default is NULL for both
                            unsorted_only = TRUE, # not default
                            random_only = TRUE, # not default
                            grouping_only = TRUE,
                            drop_na_columns = FALSE # not default
                            )

# Extra samples for a given species-survey combination
ds$x

# Unlike values
ds$u

# Summary of returns
ds$s

# All samples when any differed
ds$a
