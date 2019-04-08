library(dplyr)

context("test-get-lookup-tables")
test_that("get lookup functions work at PBS", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

d <- get_ssids()
expect_type(d$SURVEY_SERIES_DESC, "character")

d <- get_active_survey_blocks(ssid = 1)
expect_gt(d$pt1_lat[[1]], 48)
expect_gt(d$pt1_lon[[1]], -134)

d <- get_major_areas()
expect_type(d$major_stat_area_description, "character")

d <- get_management_areas()
expect_type(d$area_description, "character")
d <- d %>% filter(area_code == "4B")
expect_match(d$area_description, "4B - Strait of Georgia")

d <- get_fishery_ids()
d <- d %>% filter(fishery_id == 3)
expect_match(d$fishery_description, "Sablefish")

d <- get_species_groups()
expect_type(d$species_group_description, "character")
d <- d %>% filter(species_group_code == "RKF")
expect_match(d$species_group_description, "Rockfishes")

d <- get_gear_types()
expect_type(d$gear, "character")

d <- get_age_methods()
expect_true(!is.null(d$ageing_method_desc))

d <- get_sample_trips()
expect_true(!is.null(d$SAMPLE_ID[[1]]))

d <- get_species()
d <- d %>% filter(species_code == "442")
expect_match(d$species_common_name, "yelloweye rockfish")

d <- get_strata_areas()
expect_true(!is.null(d$SURVEY_ID[[1]]))

d <- get_survey_ids(16)
expect_true(!is.null(d$SURVEY_ID[[1]]))

d <- get_sensor_attributes()
expect_true(!is.null(d$sensor_data_attribute_desc))
expect_type(d$sensor_data_attribute_desc, "character")

d <- get_other_surveys()
expect_type(d$survey, "character")
expect_type(d$surveys_conducted_since_2008, "integer")

})
