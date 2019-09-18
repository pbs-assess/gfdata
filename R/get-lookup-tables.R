#' Lookup PBS metadata and descriptive data filters
#'
#' Extracts metadata and descriptive details useful in filtering data (i.e.
#' for extracting data only from a specific survey). Codes are often used in
#' arguments or for filtering and can be looked up with these functions.

#' @details
#' * `get_ssids()` produces a lookup table for survey series IDs and
#'    descriptions
#' * `get_age_methods()` produces a lookup table for ageing method codes
#'    and descriptions
#' * `get_sample_trips()`produces a lookup table for sample ID and
#'    fishing event ID
#' * `get_strata_areas()` produces a lookup table for surveyed area for
#'    each stratum within surveys
#' * `get_survey_ids()` produces lookup table for survey IDs for a given
#'    survey series ID
#' @name lookup

#' @export
#' @rdname lookup
get_ssids <- function() {
  .d <- run_sql(
    "GFBioSQL",
    "SELECT SURVEY_SERIES_ID, SURVEY_SERIES_DESC,
    CASE
    WHEN SURVEY_SERIES_ALT_DESC IS NULL THEN SURVEY_SERIES_TYPE_ALT_DESC
    WHEN SURVEY_SERIES_TYPE_ALT_DESC IS NULL THEN SURVEY_SERIES_ALT_DESC
    WHEN SURVEY_SERIES_TYPE_ALT_DESC = 'OTHER' THEN SURVEY_SERIES_ALT_DESC
    ELSE (SURVEY_SERIES_TYPE_ALT_DESC + ' ' + SURVEY_SERIES_ALT_DESC)
    END AS SURVEY_ABBREV
    FROM SURVEY_SERIES SS
    INNER JOIN SURVEY_SERIES_TYPE SST ON
    SST.SURVEY_SERIES_TYPE_CODE = SS.SURVEY_SERIES_TYPE_CODE"
  )
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @param ssid A numeric vector of survey series IDs. Run [get_ssids()] for a
#'   look-up table of available survey series IDs with surveys series
#'   descriptions.
#' @rdname lookup
get_active_survey_blocks <- function(ssid = NULL) {
  .q <- read_sql("get-active-survey-blocks.sql")
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  if (!is.null(ssid)) {
    .q <- inject_filter("AND SS.SURVEY_SERIES_ID IN", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  .d
}

#' @export
#' @rdname lookup
get_major_areas <- function() {
  .d <- run_sql(
    "GFFOS",
    "SELECT MAJOR_STAT_AREA_CODE, MAJOR_STAT_AREA_DESCRIPTION
    FROM MAJOR_STAT_AREA"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_management_areas <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT Area_Code, Area_Description
    FROM Area"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_fishery_ids <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT Fishery_Id, Fishery_Description
    FROM Fishery"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_species_groups <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT SG.Species_Group_Code, Species_Group_Description, Species_Code,
    Species_Common_Name
    FROM Species_Group SG
    INNER JOIN Species_Group_Species SGS ON
    SG.Species_Group_Code = SGS.Species_Group_Code"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_comm_gear_types <- function() {
  .d <- run_sql(
    "GFFOS",
    "SELECT GEAR GEAR_DESC
    FROM GFFOS.dbo.GF_MERGED_CATCH C
    GROUP BY GEAR"
  )
  names(.d) <- tolower(names(.d))
  .d$gear_desc <- tolower(.d$gear_desc)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_survey_gear_types <- function() {
  .d <- run_sql(
    "GFBioSQL",
    "SELECT GEAR_CODE, GEAR_DESC
    FROM GFBioSQL.dbo.GEAR"
  )
  names(.d) <- tolower(names(.d))
  .d$gear_desc <- tolower(.d$gear_desc)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_age_methods <- function() {
  .d <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT AGEING_METHOD_CODE, AGEING_METHOD_DESC, ROW_VERSION
    FROM AGEING_METHOD"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

get_sample_trips <- function() {
  run_sql(
    "GFBioSQL",
    "SELECT SPECIES_CODE, SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples"
  )
}

#' @export
#' @rdname lookup
get_species <- function() {
  species <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES"
  )
  names(species) <- tolower(names(species))
  mutate(species,
    species_common_name = tolower(species_common_name),
    species_science_name = tolower(species_science_name),
    parent_taxonomic_unit = tolower(parent_taxonomic_unit),
    taxonomic_rank = tolower(taxonomic_rank),
    species_grouping = tolower(species_grouping)
  ) %>%
    select(-row_version, -rsty_id, -parent_rsty_id, -species_desc) %>%
    dplyr::as.tbl()
}


#' @export
#' @rdname lookup
get_strata_areas <- function() {
  run_sql(
    "GFBioSQL",
    "SELECT SG.SURVEY_ID,
    SG.GROUPING_CODE,
    G.AREA_KM2
    FROM SURVEY_GROUPING SG
    INNER JOIN GROUPING G ON
    SG.GROUPING_CODE = G.GROUPING_CODE"
  )
}
#' @export
#' @rdname lookup
get_survey_ids <- function(ssid) {
  .q <- paste(
    "SELECT S.SURVEY_ID,
    SS.SURVEY_SERIES_ID,
    SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
    SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(ssid, collapse = ", "), ")"
  )
  run_sql("GFBioSQL", .q)
}

#' @export
#' @rdname lookup
get_sensor_attributes <- function() {
  .d <- run_sql(
    "GFBioSQL",
    "SELECT SENSOR_DATA_ATTRIBUTE_DESC
    FROM SENSOR_DATA_ATTRIBUTE"
  )
  names(.d) <- tolower(names(.d))
  .d$sensor_data_attribute_desc <- tolower(.d$sensor_data_attribute_desc)
  as_tibble(.d)
}

#' @export
#' @rdname lookup
get_other_surveys <- function() {
  .q <- read_sql("get-other-surveys.sql")
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d <- .d %>%
    filter(.data$surveys_conducted_since_2008 > 1) %>%
    select(.data$survey, .data$surveys_conducted_since_2008)
  .d
}

#' Use to get all records of any database table.
#' @param name The name of the table to get all records from.
#'     For code table use variable name without the'_code' suffix.
#' @export
#' @rdname lookup
get_table <- function(name){
  run_sql("GFBioSQL", paste("SELECT * FROM", name))
}
