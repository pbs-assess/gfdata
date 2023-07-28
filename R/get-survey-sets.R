#' Get survey set data faster and more comprehensively
#'
#' @param join_sample_ids If `TRUE` then the sample IDs will be joined in. This
#'   may result in repeated rows of data if the same sample ID is part of
#'   different survey stratifications.
#' @param verbose If `TRUE` then extra messages were reprinted during data
#'   extraction. Useful to monitor progress.
#' @param remove_false_zeros If `TRUE` will make sure weights > 0 don't have
#'   associated counts of 0 and vice versa. Only applies to trawl data where
#'   counts are only taken for small catches.
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
                            remove_false_zeros = FALSE,
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
    T.VESSEL_ID AS VESSEL_ID,
    T.CAPTAIN_ID AS CAPTAIN_ID,
    MONTH(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS MONTH,
    DAY(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS DAY,
    ISNULL(FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME) AS TIME_DEPLOYED,
    ISNULL(FE_END_BOTTOM_CONTACT_TIME, FE_BEGIN_RETRIEVAL_TIME) AS TIME_RETRIEVED,
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
                     LONGITUDE_END,
                     VESSEL_ID,
                     CAPTAIN_ID
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

  if(any(ssid %in% trawl)) {
    # calculate area_swept for trawl exactly as it has been done for the density values in this dataframe
    # note: is NA if doorspread_m is missing and duration_min may be time in water (not just bottom time)
    .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
    .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
    .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)

    # won't do this here because there may be ways of using mean(.d$doorspread_m) to fill in some NAs
    # .d <- dplyr::filter(.d, !is.na(area_swept))
    # instead use this to make sure false 0 aren't included
    .d$density_kgpm2 <- ifelse(!is.na(.d$area_swept), .d$density_kgpm2, NA)
  }

  # in trawl data, catch_count is only recorded for small catches
  # so 0 in the catch_count column when catch_weight > 0 seems misleading
  # note: there are also a few occasions for trawl where count > 0 and catch_weight is 0/NA
  # these lines replace false 0s with NA, but additional checks might be needed
  if(remove_false_zeros){
    .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
    .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)

    if(any(ssid%in%trawl)){
      .d$density_pcpm2 <- ifelse(.d$catch_count > 0 & .d$density_pcpm2 == 0, NA, .d$density_pcpm2)
      .d$density_kgpm2 <- ifelse(.d$catch_weight > 0 & .d$density_kgpm2 == 0, NA, .d$density_kgpm2)
    }
  }

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
