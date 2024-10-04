# Load
devtools::load_all()
# Source functions
source(here::here("inst", "compare", "compare-survey-sets.R"))
source(here::here("inst", "compare", "compare-survey-samples.R"))
# Define species and ssids
spp <- c("Yelloweye Rockfish", "Eulachon")
ssids <-  c(1,2,7,14,22)
# ssids <- NULL

# Compare survey sets
de <- compare_survey_sets(spp = spp,
                         ssids = ssids, # NULL here (default) returns default ssid for og fn
                         # Args for get_all_survey_sets()
                         get_all_arg_remove_false_zeros = FALSE, # not default
                         get_all_arg_remove_duplicates = TRUE, # not default
                         get_all_arg_usability = c(0, 1, 2, 6), # not default
                         get_all_arg_grouping_only = TRUE # not default
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
                            major_areas = NULL, # default is NULL for both
                            set_usability = NULL, # default is NULL for both
                            # Args for get_survey_samples()
                            get_arg_unsorted_only = TRUE, # this is default
                            # Args for get_all_survey_samples()
                            get_all_arg_unsorted_only = TRUE, # this is not default
                            get_all_arg_random_only = TRUE # other function only does this
                            )

# Extra samples for a given species-survey combination
ds$x

# Unlike values
ds$u

# Summary of returns
ds$s

# All samples when any differed
ds$a
