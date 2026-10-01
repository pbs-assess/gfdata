context("test-spatial")

test_that("load_survey_blocks() works with different datasets", {
  # Test default (syn_hbll)
  x <- load_survey_blocks()
  expect_s3_class(x, "sf")
  expect_true("survey_abbrev" %in% names(x))
  expect_true("active_block" %in% names(x))

  # Test mssm dataset
  x_mssm <- load_survey_blocks(dataset = "mssm")
  expect_s3_class(x_mssm, "sf")
  expect_true("survey_abbrev" %in% names(x_mssm))

  # Test syn_sog dataset
  x_sog <- load_survey_blocks(dataset = "syn_sog")
  expect_s3_class(x_sog, "sf")
  expect_true("survey_abbrev" %in% names(x_sog))
})

test_that("load_survey_blocks() supports synoptic/hbll subsets", {
  syn_abbrevs <- c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")
  hbll_abbrevs <- c("HBLL INS N", "HBLL INS S", "HBLL OUT N", "HBLL OUT S")

  x_syn <- load_survey_blocks(dataset = "synoptic")
  expect_setequal(unique(x_syn$survey_abbrev), syn_abbrevs)

  x_hbll <- load_survey_blocks(dataset = "hbll")
  expect_setequal(unique(x_hbll$survey_abbrev), hbll_abbrevs)

  x_ins <- load_survey_blocks(dataset = "hbll_ins")
  expect_setequal(unique(x_ins$survey_abbrev), c("HBLL INS N", "HBLL INS S"))

  x_out <- load_survey_blocks(dataset = "hbll_out")
  expect_setequal(unique(x_out$survey_abbrev), c("HBLL OUT N", "HBLL OUT S"))
})

test_that("load_survey_blocks() works with different types", {
  # Test polygon (default)
  x_poly <- load_survey_blocks(type = "polygon")
  expect_s3_class(x_poly, "sf")
  expect_true(sf::st_geometry_type(x_poly)[1] %in% c("POLYGON", "MULTIPOLYGON"))

  # Test centroid
  expect_warning(x_centroid <- load_survey_blocks(type = "centroid"),
                 "st_point_on_surface assumes attributes are constant")
  expect_s3_class(x_centroid, "sf")
  expect_true(sf::st_geometry_type(x_centroid)[1] %in% c("POINT", "MULTIPOINT"))

  # Test XY coordinates
  expect_warning(x_xy <- load_survey_blocks(type = "XY"),
                 "st_point_on_surface assumes attributes are constant")
  expect_s3_class(x_xy, "tbl_df")
  expect_true("X" %in% names(x_xy))
  expect_true("Y" %in% names(x_xy))
  expect_false("geometry" %in% names(x_xy))
})

test_that("load_survey_blocks() dataset and type matching is case-insensitive", {
  expect_warning(
    x_upper <- load_survey_blocks(dataset = "SYNOPTIC", type = "XY"),
    "st_point_on_surface assumes attributes are constant"
  )
  expect_warning(
    x_lower <- load_survey_blocks(dataset = "synoptic", type = "XY"),
    "st_point_on_surface assumes attributes are constant"
  )
  expect_equal(x_upper, x_lower)
})

test_that("load_survey_blocks() XY output carries a crs attribute", {
  expect_warning(
    x <- load_survey_blocks(type = "XY"),
    "st_point_on_surface assumes attributes are constant"
  )
  expect_equal(sf::st_crs(attr(x, "crs"))$epsg, 32609L)
})

test_that("load_survey_blocks() active_only parameter works", {
  # Test with active_only = TRUE (default) - using syn_hbll which has active_block
  x_active <- load_survey_blocks(dataset = "syn_hbll", active_only = TRUE)
  expect_true(all(x_active$active_block, na.rm = TRUE))

  # Test with active_only = FALSE
  x_all <- load_survey_blocks(dataset = "syn_hbll", active_only = FALSE)
  expect_gte(nrow(x_all), nrow(x_active))

  # Test that MSSM dataset works (it doesn't have active_block column)
  x_mssm <- load_survey_blocks(dataset = "mssm", active_only = TRUE)
  expect_s3_class(x_mssm, "sf")
  expect_false("active_block" %in% names(x_mssm))

  # Test that MSSM dataset ignores active_only parameter
  x_mssm_all <- load_survey_blocks(dataset = "mssm", active_only = FALSE)
  expect_equal(nrow(x_mssm), nrow(x_mssm_all))  # Should be same regardless of active_only
})

test_that("load_survey_blocks() handles invalid inputs", {
  expect_error(load_survey_blocks(dataset = "invalid"), "should be one of")
  expect_error(load_survey_blocks(type = "invalid"), "should be one of")
})

test_that("sql_geom_to_sf() works", {
  # Create test data with polygon coordinates
  test_data <- data.frame(
    pt1_lon = c(-123, -122),
    pt1_lat = c(48, 49),
    pt2_lon = c(-122, -121),
    pt2_lat = c(48, 49),
    pt3_lon = c(-122, -121),
    pt3_lat = c(49, 50),
    pt4_lon = c(-123, -122),
    pt4_lat = c(49, 50),
    other_col = c("A", "B")
  )

  result <- sql_geom_to_sf(test_data, crs = 4326)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_true("geometry" %in% names(result))
  expect_true("other_col" %in% names(result))
  expect_false(any(grepl("^pt", names(result))))
  expect_equal(sf::st_crs(result)$epsg, 4326)
})

test_that("sql_geom_to_sf() validates input and warns about CRS issues", {
  # Test CRS warning for coordinates that look like UTM
  utm_like_data <- data.frame(
    pt1_lon = 500000, pt1_lat = 5000000,  # Looks like UTM
    pt2_lon = 501000, pt2_lat = 5000000,
    pt3_lon = 501000, pt3_lat = 5001000,
    pt4_lon = 500000, pt4_lat = 5001000
  )
  expect_warning(sql_geom_to_sf(utm_like_data, crs = 4326),
                 "Coordinates appear to be outside WGS84 longitude range")
})

test_that("sql_geom_to_sf() works with UTM coordinates and correct CRS", {
  # Example with UTM coordinates (like in documentation)
  utm_data <- data.frame(
    pt1_lon = 500000, pt1_lat = 5000000,
    pt2_lon = 501000, pt2_lat = 5000000,
    pt3_lon = 501000, pt3_lat = 5001000,
    pt4_lon = 500000, pt4_lat = 5001000,
    site_id = "A1"
  )

  result <- sql_geom_to_sf(utm_data, crs = 32609)  # Correct CRS for UTM

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true("site_id" %in% names(result))
  expect_false(any(grepl("^pt", names(result))))
  expect_equal(sf::st_crs(result)$epsg, 32609)
})
