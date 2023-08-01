#' Get survey set data faster and more comprehensively
#'
#' @param years If NULL, returns all years.
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
#' ## `area_swept` is in m^2 and is used to calculate density for trawl surveys
#' ## It is based on `area_swept1` (`doorspread_m` x `tow_length_m`) except
#' ## when `tow_length_m` is missing, and then we use `area_swept2`
#' ## (`doorspread` x `duration_min` x `speed_mpm`).
#' ## `duration_min` is derived in the SQL procedure "proc_catmat_2011" and
#' ## differs slightly from the difference between `time_deployed` and
#' ## `time_retrieved`.
#' }
#'
get_survey_sets2 <- function(species,
                             ssid = c(1, 3, 4, 16, 2, 14, 22, 35, 36, 39, 40),
                             years = NULL,
                            join_sample_ids = FALSE, verbose = FALSE,
                            remove_false_zeros = FALSE,
                            usability = c(0,1,2,6)
                            ) {




  .q <- read_sql("get-survey-sets.sql")

  if (!is.null(species)) {
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  }

  if (!is.null(ssid)) {

    survey_ids <- get_survey_ids(ssid)
    .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
                        sql_code = .q,
                        search_flag = "-- insert ssid here", conversion_func = I
    )
  }

  # if (!is.null(major)) {

  #   .q <- inject_filter("AND SM.MAJOR_STAT_AREA_CODE =", major, .q,
  #                       search_flag = "-- insert major here", conversion_func = I
  #   )
  # }


  .d <- run_sql("GFBioSQL", .q)

  if (!is.null(years)) {
    .d <- filter(.d, YEAR %in% years)
    }


  if (join_sample_ids) {
    # give us each sample_id associated with each fishing_event_id and species:

    sample_trip_ids <- get_sample_trips()
    areas <- get_strata_areas()

    .d <- left_join(.d, sample_trip_ids,
                    by = c("SPECIES_CODE", "FISHING_EVENT_ID")
    ) %>%
      left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))

    warning(
      "Adding sample_id will duplicate some fishing events.",
      "This occurs when the same species was assigned two distinct sample ids."
    )
  }


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


  if (nrow(.d) < 1) {
    stop("No survey set data for selected species.")
  }


  names(.d) <- tolower(names(.d))


  # get all fishing event info
  .fe <- read_sql("get-event-data.sql")

  if (!is.null(ssid)) {

    #survey_ids <- get_survey_ids(ssid)
    .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
                        sql_code = .fe,
                        search_flag = "-- insert ssid here", conversion_func = I
    )
  }

  fe <- run_sql("GFBioSQL", .fe)


  if (is.null(ssid)) {
    fe <- filter(fe, SURVEY_SERIES_ID > 0)
    # TODO: something to address replicated events across different survey series
  }
# browser()

  if(all(ssid %in% trawl)) {

    names(fe) <- tolower(names(fe))

    exdat <- expand.grid(fishing_event_id = unique(fe$fishing_event_id), species_code = unique(.d$species_code))

    .d <- left_join(exdat,
                     unique(select(
                       fe,
                       #-survey_id,
                       #-survey_series_id,
                       -fe_parent_event_id,
                       -fe_minor_level_id,
                       -hook_code,
                       -lglsp_hook_count
                     ))
    ) %>% left_join(.d)

  } else {

    fe_A_no_parent <- filter(fe, is.na(FE_PARENT_EVENT_ID)) # just actual events

    fe_B_no_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), is.na(FE_MINOR_LEVEL_ID)) %>%
      group_by(FE_PARENT_EVENT_ID, SURVEY_ID, SURVEY_SERIES_ID) %>%
      mutate(SKATE_COUNT = n()) %>% select(FE_PARENT_EVENT_ID, FISHING_EVENT_ID, SURVEY_ID, SURVEY_SERIES_ID, SKATE_COUNT) %>% distinct() %>%
      rename(skate_id = FISHING_EVENT_ID, fishing_event_id = FE_PARENT_EVENT_ID) %>% ungroup()

    # fe_C <- filter(fe_w_parent_events, !is.na(FE_MINOR_LEVEL_ID))
    fe_C_w_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), !is.na(FE_MINOR_LEVEL_ID)) %>%
      group_by(FE_PARENT_EVENT_ID, SURVEY_ID, SURVEY_SERIES_ID) %>%
      mutate(MINOR_ID_COUNT = n(),
             MINOR_ID_MAX = max(FE_MINOR_LEVEL_ID, na.rm = TRUE)) %>%
      select(FE_PARENT_EVENT_ID, SURVEY_ID, SURVEY_SERIES_ID, MINOR_ID_COUNT, MINOR_ID_MAX) %>% distinct() %>%
      rename(skate_id = FE_PARENT_EVENT_ID) %>% ungroup()

    sub_event_counts <- full_join(
      fe_B_no_minor,
      fe_C_w_minor
    )

    missing_event_ids <- filter(sub_event_counts, is.na(fishing_event_id)) %>% mutate(fishing_event_id = skate_id)

    sub_event_counts2 <- full_join(filter(sub_event_counts, !is.na(fishing_event_id)), missing_event_ids)

    sub_event_counts3 <- sub_event_counts2 %>%
      group_by(fishing_event_id, SURVEY_ID, SURVEY_SERIES_ID) %>%
      reframe(
        skate_count = mean(SKATE_COUNT, na.rm = T),
        mean_per_skate = mean(MINOR_ID_COUNT, na.rm = T),
        minor_id_count = sum(MINOR_ID_COUNT, na.rm = T),
        minor_id_max = max(MINOR_ID_MAX, na.rm = T)
      ) %>% distinct() %>%
      mutate(diff = ifelse(minor_id_max > 0, minor_id_max-minor_id_count, NA)) %>%
      select(-SURVEY_ID, -SURVEY_SERIES_ID) %>% distinct()

    check_for_duplicates <- sub_event_counts3[duplicated(sub_event_counts3$fishing_event_id), ] # none :)


    # all_top_events <- select(fe_A_no_parent, FISHING_EVENT_ID, SURVEY_ID, SURVEY_SERIES_ID) %>% distinct() %>%


    fe2 <- fe %>% rename(fishing_event_id = FISHING_EVENT_ID) %>% left_join(sub_event_counts3)

    names(fe2) <- tolower(names(fe2))

    if(!all(ssid %in% trawl)) {


      exdat <- expand.grid(fishing_event_id = unique(fe2$fishing_event_id), species_code = unique(.d$species_code))

      .d <- left_join(exdat,
                     unique(select(
                       fe2,
                       #-survey_id,
                       #-survey_series_id,
                       -fe_parent_event_id,
                       -fe_minor_level_id,
                       -tow_length_m,
                       -Mouth_width_m,
                       -doorspread_m,
                       -speed_mpm,
                       -grouping_code_trawl
                     ))
    ) %>% left_join(.d)
    } else {
      exdat <- expand.grid(fishing_event_id = unique(fe2$fishing_event_id), species_code = unique(.d$species_code))
      .d <- left_join(exdat,

                       unique(select(
                          fe2,
                       #  -survey_id,
                       #  -survey_series_id,
                          -fe_parent_event_id,
                          -fe_minor_level_id
                       ))
      ) %>% left_join(.d)
      }
  }


  surveys <- get_ssids()
  names(surveys) <- tolower(names(surveys))
  .d <- inner_join(.d,
                   unique(select(
                     surveys,
                     survey_series_id,
                     survey_series_desc,
                     survey_abbrev
                   )),
                   by = "survey_series_id"
  )

  species_df <- run_sql("GFBioSQL", "SELECT * FROM SPECIES") %>%
    select(
    SPECIES_CODE,
    SPECIES_COMMON_NAME,
    SPECIES_SCIENCE_NAME,
    SPECIES_DESC
  )
  names(species_df) <- tolower(names(species_df))

  .d <- inner_join(.d,
                   unique(species_df),
                   by = "species_code"
  )


  # create zeros
    .d$catch_count <- ifelse(is.na(.d$catch_count), 0, .d$catch_count)
    .d$catch_weight <- ifelse(is.na(.d$catch_weight), 0, .d$catch_weight)

  # in trawl data, catch_count is only recorded for small catches
  # so 0 in the catch_count column when catch_weight > 0 seems misleading
  # note: there are also a few occasions for trawl where count > 0 and catch_weight is 0/NA
  # these lines replace false 0s with NA, but additional checks might be needed
  if(remove_false_zeros){
    .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
    .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)

    # if(any(ssid%in%trawl)){
    #   .d$density_pcpm2 <- ifelse(.d$catch_count > 0 & .d$density_pcpm2 == 0, NA, .d$density_pcpm2)
    #   .d$density_kgpm2 <- ifelse(.d$catch_weight > 0 & .d$density_kgpm2 == 0, NA, .d$density_kgpm2)
    # }
  }


  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  } else {

    u <- get_table("usability")

    names(u) <- tolower(names(u))
    .d <- left_join(.d,
                    unique(select(
                      u,
                      usability_code,
                      usability_desc
                    ))
    )

  }

  if(any(ssid %in% trawl)) {
    # calculate area_swept for trawl exactly as it has been done for the density values in this dataframe
    # note: is NA if doorspread_m is missing and duration_min may be time in water (not just bottom time)
    .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
    .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
    .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)
    .d$area_swept_km2 <- .d$area_swept/1000000
    # won't do this here because there may be ways of using mean(.d$doorspread_m) to fill in some NAs
    # .d <- dplyr::filter(.d, !is.na(area_swept))
    # instead use this to make sure false 0 aren't included
    .d$density_kgpm2 <- .d$catch_weight/.d$area_swept
    .d$density_kgpm2 <- ifelse(!is.na(.d$area_swept), .d$density_kgpm2, NA)
  }

  if(any(ssid %in% ll)) {
    .d$hook_area_swept_km2 <- ifelse(.d$survey_series_id == 14,
                                     0.0054864 * 0.009144 * .d$minor_id_count,
                                     0.0024384 * 0.009144 * .d$minor_id_count)
    .d$density_pppm2 <- .d$catch_count/(.d$hook_area_swept_km2*1000000)
  }

  .d <- mutate(.d,
               species_science_name = tolower(species_science_name),
               species_desc = tolower(species_desc),
               species_common_name = tolower(species_common_name)
  )
  #.d <- unique(.d)
  species_codes <- common2codes(species)
  missing_species <- setdiff(species_codes, .d$species_code)
  if (length(missing_species) > 0) {
    warning(
      "The following species codes do not have survey set data in GFBio.",
      paste(missing_species, collapse = ", ")
    )
  }
  add_version(as_tibble(.d))
}
