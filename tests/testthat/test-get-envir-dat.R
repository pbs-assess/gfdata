context("test-get-envir-dat")

test_that("environmental data extraction works", {
  # skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  tryCatch(get_ssids(), error = function(e) skip("No database access"))

  d <- get_sensor_data_trawl(ssid = 1, "temperature")
  expect_equal(class(d$count), "integer")
  expect_equal(length(d), 14)

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
  expect_type(d$`dissolved oxygen_mlpL`, "double")

})
