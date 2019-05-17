context("test-get-spp-dat")

library(dplyr)

test_that("get species data functions work at PBS", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  tryCatch(get_ssids(), error = function(e) skip("No database access"))

  expect_error(get_survey_sets("lingcod", ssid = 99999))

  d <- get_survey_sets("lingcod", 16)
  expect_gte(d$catch_weight[[1]], 0)

  d <- get_survey_samples("lingcod", 16)
  expect_type(d$survey_id, "integer")

  d <- get_commercial_samples("lingcod")
  expect_gte(d$year[[1]], 1900L)

  d <- get_catch("lingcod")
  expect_gte(nrow(d), 1L)

  d <- get_hake_catch()
  expect_false(is.null(sum(d$landed_kg)))
  expect_match(class(d$discarded_kg), "numeric")

  d <- get_cpue_historical("442")
  expect_equal(d$species_common_name[[1]], "YELLOWEYE ROCKFISH")
  expect_equal(class(d$landed_kg), "numeric")

  d <- get_cpue_spatial("lingcod")
  expect_gte(d$fishing_event_id[[1]], 1)
  expect_equal(class(d$cpue), "numeric")

  d <- get_cpue_spatial_ll("lingcod")
  expect_match(class(d$landed_round_kg), "numeric")
  expect_gte(nrow(d), 1L)

  d <- get_cpue_index("bottom trawl", min_cpue_year = 2015)
  expect_gt(sum(d$landed_kg, d$discarded_kg), 0)

  d <- get_age_precision("lingcod")
  expect_equal(d$species_code[[1L]], "467")

  d <- get_survey_index("lingcod", ssid = 1)
  expect_type(d$num_pos_sets, "integer")

  d <- get_management(396)
  expect_match(d$species_common_name, "pacific ocean perch")

  d <- get_sara_dat()
  expect_type(d$sara_status, "character")

})
