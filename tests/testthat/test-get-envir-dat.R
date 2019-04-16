context("test-get-envir-dat")

library(dplyr)

test_that("environmental data extraction works", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  d <- get_sensor_data_trawl(ssid = 1, "temperature")
  expect_equal(class(d$count), "integer")
  expect_equal(length(d), 12)

  d <- get_sensor_data_trawl_fe(d$fishing_event_id[[1]], "temperature")
  expect_equal(class(d$sensor_data_value), "numeric")
  expect_equal(length(d), 6)

  d <- get_sensor_data_ll_td(ssid = 40)
  expect_equal(sum(is.null(d$fishing_event_id)), 0)
  expect_type(d$survey_desc, "character")

  d <- get_sensor_data_ll_td_fe(d$fishing_event_id[[1]],"temperature")
  expect_equal(length(d), 8)
  expect_type(d$sensor_data_value, "double")

  d <- get_sensor_data_ll_ctd(40)
  expect_equal(ncol(d), 14)
  expect_type(d$`dissolved oxygen_mlpL`, "")

})





#
# get_sensor_data_trawl <- function(ssid = NULL,
#   attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
#   spread_attributes = FALSE)
