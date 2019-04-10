context("test-get-envir-dat")

library(dplyr)

test_that("environmental data extraction works", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  d <- get_sensor_data_trawl(1, "temperature")
  expect_equal(class(d$count), "integer")
  expect_equal(length(d), 12)

  d <- get_sensor_data_trawl_fe(308673, "temperature")
  expect_equal(class(d$sensor_data_value), "numeric")
  expect_equal(length(d), 6)

  d <- get_sensor_data_ll_td(ssid = 40)
  expect_equal(sum(is.null(d$fishing_event_id)), 0)
  expect_type(d$survey_desc, "character")

})





#
# get_sensor_data_trawl <- function(ssid = NULL,
#   attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
#   spread_attributes = FALSE)
