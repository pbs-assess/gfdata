# Load
devtools::load_all()
# Source functions
source(here::here("inst", "compare", "compare-survey-sets.R"))
source(here::here("inst", "compare", "compare-survey-samples.R"))
# Define species and ssids
spp <- c("Yelloweye Rockfish", "Eulachon")
ssids <-  c(#1,2,7,14,
            22)
# ssids <- NULL

# Compare survey sets
de <- compare_survey_sets(spp = spp,
                         ssids = ssids # NULL here (default) returns default ssid for og fn
                         # # Args for get_all_survey_sets()
                         # remove_false_zeros = FALSE, # not default
                         # usability = c(0, 1, 2, 6) # not default
                         # grouping_only = TRUE # not default
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
                            major = NULL, # default is NULL for both
                            usability = NULL # default is NULL for both
                            )

# Extra samples for a given species-survey combination
ds$x

# Unlike values
ds$u

# Summary of returns
ds$s

# All samples when any differed
ds$a
