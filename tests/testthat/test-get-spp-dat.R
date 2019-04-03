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

  # d <- get_hake_catch()
  #
  # d <- get_cpue_historical()
  #
  # d <- get_cpue_spatial("lingcod")
  #
  # d <- get_cpue_spatial_ll("lingcod")
  #
  # d <- get_cpue_index("bottom trawl", min_cpue_year = 2015)

  d <- get_age_precision("lingcod")
  expect_equal(d$species_code[[1L]], "467")

  d <- get_survey_index("lingcod", ssid = 1)
  expect_type(d$num_pos_sets, "integer")

  # d <- get_management()

})
