context("test-utils")


test_that("run_sql works", {
  skip_on_travis()
  skip_on_appveyor()
  tryCatch(get_ssids(), error = function(e) skip("No database access"))
  .q <- paste(
    "SELECT DISTINCT(SS.SURVEY_SERIES_ID),
    SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
    SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (1, 3, 4, 16)"
  )
  x <- run_sql("GFBioSQL", .q)


  expect_gte(nrow(x), 4)
  expect_identical(class(x), "data.frame")
})

# db_connection()

test_that("force_three_letter_species_code() works", {
  expect_equal(force_three_letter_species_code(3), "003")
  expect_equal(force_three_letter_species_code(c(1, 2)), c("001", "002"))
  expect_equal(force_three_letter_species_code(03), "003")
  expect_equal(force_three_letter_species_code("4004"), "4004")
  expect_equal(force_three_letter_species_code("003"), "003")
  expect_equal(force_three_letter_species_code("abc"), "abc")
})

test_that("collapse_filters() works", {
  expect_equal(collapse_filters(c(1, 2)), "'1','2'")
})

test_that("inject_filter() works", {
  x <- inject_filter("a",
    species = "b",
    sql_code = list("y", "\n-- insert here", "z"),
    search_flag = "-- insert here",
    conversion_func = I
  )

  expect_equal(x[[1]], "y")
  expect_equal(x[[2]], "a ('b')")
  expect_equal(x[[3]], "z")
})

test_that("first_cap() works", {
  expect_equal(first_cap("abc def"), "Abc Def")
})

test_that("read_sql works", {
  x <- read_sql("get-catch.sql")
  expect_gte(length(x), 1)
  expect_type(x, "character")
})

test_that("all_species_codes() works", {
  expect_false(all_species_codes("a"))
  expect_false(all_species_codes(c("a", "b")))
  expect_true(all_species_codes(1))
  expect_true(all_species_codes(c(1, 1)))
  expect_true(all_species_codes(c("1", "2")))
  expect_false(all_species_codes(c("a", 1)))
  expect_false(all_species_codes(c("a", "1")))
  expect_false(all_species_codes(c("a", "a1")))
})

test_that("common2codes() works", {
  expect_equal(common2codes(3), "003")
  expect_equal(common2codes(c(1, 2)), c("001", "002"))
  expect_equal(common2codes(c("001", "002")), c("001", "002"))
})

test_that("codes2common() works", {
  tryCatch(get_ssids(), error = function(e) skip("No database access"))
  expect_message(codes2common(465), "Code 465 deprecated for Lingcod. Use code 467.")
  expect_equal(codes2common("222"), "PACIFIC COD")
})
