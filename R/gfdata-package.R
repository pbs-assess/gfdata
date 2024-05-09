#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

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
      "DAY",
      "MONTH",
      "TIME_DEPLOYED",
      "TIME_RETRIEVED",
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
      "FISHING_EVENT_ID",
      "LATITUDE_END",
      "LONGITUDE_END",
      "fork_length",
      "second_dorsal_length",
      "standard_length",
      "total_length",
      "unit",
      "usability_code",
      "value",
      "year",
      "SURVEY_ABBREV",
      "selection_ind"
    )
  )
}
