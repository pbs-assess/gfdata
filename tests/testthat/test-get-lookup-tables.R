library(dplyr)

context("test-get-lookup-tables")
test_that("get lookup functions work at PBS", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

d <- get_ssids()
expect_type(d$SURVEY_SERIES_DESC, "character")

# get_major_areas
#
# get_management_areas
#
# get_fishery_ids
#
# get_species_groups
#
# get_gear_types
#
d <- get_age_methods()
expect_true(!is.null(d$ageing_method_desc))

d <- get_sample_trips()
expect_true(!is.null(d$SAMPLE_ID[[1]]))

# get_species

d <- get_strata_areas()
expect_true(!is.null(d$SURVEY_ID[[1]]))

d <- get_survey_ids(16)
expect_true(!is.null(d$SURVEY_ID[[1]]))

# get_sensor_attributes
#
# get_other_surveys

})
