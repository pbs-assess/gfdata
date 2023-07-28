#' Get survey set data faster and more comprehensively
#'
#' @param join_sample_ids If `TRUE` then the sample IDs will be joined in. This
#'   may result in repeated rows of data if the same sample ID is part of
#'   different survey stratifications.
#' @param verbose If `TRUE` then extra messages were reprinted during data
#'   extraction. Useful to monitor progress.
#' @param sleep System sleep in seconds between each survey-year
#'   to be kind to the server.
#' @export
#' @rdname get_data
#' @examples
#' \dontrun{
#' ## Import survey catch density and location data by tow or set for plotting
#' ## Specify single or multiple species by common name or species code and
#' ## single or multiple survey series id(s).
#' ## Notes:
#' ## `area_km` is the stratum area used in design-based index calculation.
#' ## `area_swept` is used to calculate density for trawl surveys and based on
#' ## `area_swept1` (`doorspread_m` x `tow_length_m`) except when
#' ## `tow_length_m` is missing, and then we use `area_swept2`
#' ## (`doorspread` x `duration_min` x `speed_mpm`).
#' ## `duration_min` is derived in the SQL procedure "proc_catmat_2011" and
#' ## differs slightly from the difference between `time_deployed` and
#' ## `time_retrieved`.
#'
get_survey_sets2 <- function(species, ssid = c(1, 3, 4, 16, 2, 14, 22, 36, 39, 40),
                            join_sample_ids = FALSE, verbose = FALSE,
                            sleep = 0.05) {
  # Just to pull out up to date list of ssids associated with trawl/ll gear type.
  trawl <- run_sql("GFBioSQL", "SELECT
    S.SURVEY_SERIES_ID
    FROM SURVEY_SERIES SS
    LEFT JOIN SURVEY S ON S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
    LEFT JOIN TRIP_SURVEY TS ON TS.SURVEY_ID = S.SURVEY_ID
    LEFT JOIN FISHING_EVENT FE ON FE.TRIP_ID = TS.TRIP_ID
    WHERE GEAR_CODE IN(1, 6, 8, 11, 14, 16) AND
    S.SURVEY_SERIES_ID <> 0
    GROUP BY S.SURVEY_SERIES_ID, [SURVEY_SERIES_DESC]
    ,[SURVEY_SERIES_TYPE_CODE]
    ,[SURVEY_SERIES_ALT_DESC],
    TRAWL_IND, GEAR_CODE
    ORDER BY S.SURVEY_SERIES_ID")
  trawl <- unique(trawl$SURVEY_SERIES_ID)

  ll <- run_sql("GFBioSQL", "SELECT
    S.SURVEY_SERIES_ID
    FROM SURVEY_SERIES SS
    LEFT JOIN SURVEY S ON S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
    LEFT JOIN TRIP_SURVEY TS ON TS.SURVEY_ID = S.SURVEY_ID
    LEFT JOIN FISHING_EVENT FE ON FE.TRIP_ID = TS.TRIP_ID
    WHERE GEAR_CODE IN(4,5,7,10,12) AND
    S.SURVEY_SERIES_ID <> 0
    GROUP BY S.SURVEY_SERIES_ID, [SURVEY_SERIES_DESC]
    ,[SURVEY_SERIES_TYPE_CODE]
    ,[SURVEY_SERIES_ALT_DESC],
    TRAWL_IND, GEAR_CODE
    ORDER BY S.SURVEY_SERIES_ID")
  ll <- unique(ll$SURVEY_SERIES_ID)

  missing <- setdiff(ssid, c(trawl, ll))
  if (length(missing) > 0) {
    stop("ssid(s) ", missing, " is/are not supported. Must be one of ",
         paste(sort(c(trawl, ll)), collapse = ", "), ". ",
         "See the function `get_ssids()` for help identifying ",
         "survey series IDs.",
         call. = FALSE
    )
  }

  species_codes <- common2codes(species)

  species_df <- run_sql("GFBioSQL", "SELECT * FROM SPECIES")
  sample_trip_ids <- get_sample_trips()
  areas <- get_strata_areas()
  survey_ids <- get_survey_ids(ssid)
  surveys <- get_ssids()

  ## STILL NEED TO ADD
  # CAPTAIN_ID,
  # VESSEL_ID,

  fe <- run_sql("GFBioSQL", "SELECT
    FISHING_EVENT_ID,
    MONTH(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS MONTH,
    DAY(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS DAY,
    FE_END_DEPLOYMENT_TIME AS TIME_DEPLOYED,
    FE_BEGIN_RETRIEVAL_TIME AS TIME_RETRIEVED,
    FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS
      LONGITUDE,
    FE_END_LATTITUDE_DEGREE + FE_END_LATTITUDE_MINUTE / 60 AS LATITUDE_END,
    -(FE_END_LONGITUDE_DEGREE + FE_END_LONGITUDE_MINUTE / 60) AS
      LONGITUDE_END,
      FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
    FROM FISHING_EVENT FE
    INNER JOIN TRIP T ON T.TRIP_ID = FE.TRIP_ID")

  Sys.sleep(sleep)

  d_survs <- list()
  k <- 0
  for (i in seq_along(species_codes)) {
    for (j in seq_along(survey_ids$SURVEY_ID)) {
      if (survey_ids$SURVEY_SERIES_ID[j] %in% trawl) {
        sql_proc <- "proc_catmat_2011"
      }
      if (survey_ids$SURVEY_SERIES_ID[j] %in% ll) {
        sql_proc <- "proc_catmat_ll_2013"
      }

      k <- k + 1
      if (verbose) {
        message(
          "extracting data for survey ID ", survey_ids$SURVEY_ID[j],
          " and species code ", species_codes[i]
        )
      }
      d_survs[[k]] <- run_sql(
        "GFBioSQL",
        paste0(
          "EXEC ", sql_proc, " ", survey_ids$SURVEY_ID[j], ", '",
          species_codes[i], "'"
        )
      )
      Sys.sleep(sleep)
    }
  }
  .d <- bind_rows(d_survs)

  if (nrow(.d) < 1) {
    stop("No survey set data for selected species.")
  }

  .d <- inner_join(.d,
                   unique(select(
                     surveys,
                     SURVEY_SERIES_ID,
                     SURVEY_SERIES_DESC,
                     SURVEY_ABBREV
                   )),
                   by = "SURVEY_SERIES_ID"
  )

  .d <- inner_join(.d,
                   unique(select(
                     fe,
                     FISHING_EVENT_ID,
                     MONTH,
                     DAY,
                     TIME_DEPLOYED,
                     TIME_RETRIEVED,
                     LATITUDE_END,
                     LONGITUDE_END
                   )),
                   by = "FISHING_EVENT_ID"
  )

  .d <- inner_join(.d,
                   unique(select(
                     species_df,
                     SPECIES_CODE,
                     SPECIES_COMMON_NAME,
                     SPECIES_SCIENCE_NAME,
                     SPECIES_DESC
                   )),
                   by = "SPECIES_CODE"
  )

  if (join_sample_ids) {
    # give us each sample_id associated with each fishing_event_id and species:
    .d <- left_join(.d, sample_trip_ids,
                    by = c("SPECIES_CODE", "FISHING_EVENT_ID")
    ) %>%
      left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))
  }

  names(.d) <- tolower(names(.d))
  .d <- mutate(.d,
               species_science_name = tolower(species_science_name),
               species_desc = tolower(species_desc),
               species_common_name = tolower(species_common_name)
  )

  missing_species <- setdiff(species_codes, .d$species_code)
  if (length(missing_species) > 0) {
    warning(
      "The following species codes do not have survey set data in GFBio.",
      paste(missing_species, collapse = ", ")
    )
  }
  add_version(as_tibble(.d))
}

#' @export
#'
#' @rdname get_data
get_ll_hook_data <- function(species = NULL, ssid = NULL){
  .q <- read_sql("get-ll-hook-data.sql")
  .q <- inject_filter("", species, sql_code = .q)
  if (!is.null(ssid)) {
    .q <- inject_filter(" ", ssid,
                        sql_code = .q,
                        search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  # .d$species_common_name <- tolower(.d$species_common_name)
  # .d$species_science_name <- tolower(.d$species_science_name)

  add_version(as_tibble(.d))
}

