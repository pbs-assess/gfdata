#' gfdata package
#'
#' @description An R package to facilitate extracting ground fish data at PBS
#'
#' @docType package
#' @name gfdata

#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
NULL

if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".time_diff",
      ".year_start_date",
      "SPECIES_CODE",
      "SPECIES_COMMON_NAME",
      "SPECIES_DESC",
      "SPECIES_SCIENCE_NAME",
      "SURVEY_ABBREV ",
      "SURVEY_SERIES_DESC",
      "SURVEY_SERIES_ID",
      "action_start_date",
      "avg",
      "best_date",
      "count",
      "parameter",
      "parent_rsty_id",
      "parent_taxonomic_unit",
      "row_version",
      "rsty_id",
      "sampling_desc",
      "species_ageing_group",
      "species_code",
      "species_common_name",
      "species_desc",
      "species_grouping",
      "species_science_name",
      "taxonomic_rank",
      "totcatch_kg",
      "trip_start_date",
      "unit",
      "usability_code",
      "value",
      "year",
      "SURVEY_ABBREV"
    )
  )
}
