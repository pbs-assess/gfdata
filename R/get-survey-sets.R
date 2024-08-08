#' Get survey set data faster and more comprehensively
#'
#' @param years If NULL, returns all years.
#' @param join_sample_ids This option was problematic, so now reverts to FALSE.
#' @param verbose Doesn't do anything in this version.
#' @param remove_false_zeros If `TRUE` will make sure weights > 0 don't have
#'   associated counts of 0 and vice versa. Only applies to trawl data where
#'   counts are only taken for small catches.
#' @param remove_duplicates Remove duplicated event records due to overlapping survey
#'   stratifications when original_ind = 'N', or from known issues with MSSM trips including both survey areas.
#' @param include_activity_matches Get all surveys with activity codes that match chosen ssids.
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
get_all_survey_sets <- function(species,
                             ssid = c(1, 3, 4, 16, 2, 14, 22, 35, 36, 39, 40),
                             years = NULL,
                             join_sample_ids = FALSE, verbose = FALSE,
                             remove_false_zeros = FALSE,
                             remove_duplicates = FALSE,
                             include_activity_matches = FALSE,
                             usability = c(0, 1, 2, 6)) {

  .q <- read_sql("get-survey-sets.sql")

  if (!is.null(species)) {
    .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  }


  if (!is.null(ssid)) {
    if (include_activity_matches){
      ## draft approach that gets all samples collected using the same activities as the ssid(s) of interest
      .a <- read_sql("get-activity-code.sql")
      .a <- run_sql("GFBioSQL", .a)

      .a <-  filter(.a, SURVEY_SERIES_ID %in% ssid) |> distinct()

      activities <- unique(.a$ACTIVITY_CODE)
      .q <- inject_filter("AND TA.ACTIVITY_CODE IN", activities,
                          sql_code = .q,
                          search_flag = "-- insert ssid here", conversion_func = I
      )
    } else {
    survey_ids <- get_survey_ids(ssid)
    .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
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
    # sample_trip_ids <- get_sample_trips()
    # areas <- get_strata_areas()
    #
    # .d <- left_join(.d, sample_trip_ids,
    #   by = c("SPECIES_CODE", "FISHING_EVENT_ID")
    # ) %>%
    #   left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))

    warning(
      "The join_sample_ids option has been removed. To bind with ",
      "sample data, it is safer to use the include_event_info = TRUE ",
      "option in get_all_survey_samples() instead."
    )
  }

  # Just to pull out up to date list of ssids associated with trawl/ll gear type.
  # Sys.sleep(0.05) # might be useful if server has difficulty

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

  # Sys.sleep(0.05)

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


  # whenever ssid is included, but not activity matches
  # we need to drop duplicated records from trips that include multiple surveys
  if (!include_activity_matches & !is.null(ssid)) {
    .d <- filter(.d, (survey_series_id %in% c(ssid)))
  }

  # if using include_activity_matches = TRU`then remove_duplicates = TRUE
  if (include_activity_matches & !is.null(ssid)) {
    remove_duplicates <- TRUE
  }


  # check if there are duplicate fishing_event ids
  if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {

    if (remove_duplicates) {
      # add custom fixes for problem surveys here:
      # first split data into unique fishing_events (dd1) and ones with duplicates (dd2)

      .dd <- .d[duplicated(.d$fishing_event_id),]
      dd1 <- filter(.d, !(fishing_event_id %in% c(unique(.dd$fishing_event_id))))
      dd2 <- filter(.d, (fishing_event_id %in% c(unique(.dd$fishing_event_id))))

      # then only applying fixes to duplicated fishing_events in case some are miss-assigned but not duplicated cases
      # for shrimp survey sets in both qcs and wcvi that were done on the same trip they get duplicated by the sql call
      # note: getting some that violate these rules but aren't duplicated... eg. fe 1720260, 1720263
      dd2 <- dd2[which(!(dd2$survey_series_id == 6 & dd2$major_stat_area_code %in% c("03", "04"))),]
      dd2 <- dd2[which(!(dd2$survey_series_id == 7 & dd2$major_stat_area_code %in% c("05", "06"))),]

      # for sablefish sets with inlets and offshore on same trip ???
      # dd2 <- dd2[which(!(dd2$survey_series_id == ## & dd2$reason %in% c(""))),]
      # dd2 <- dd2[which(!(dd2$survey_series_id == ## & dd2$reason %in% c(""))),]

      # for dogfish and HBLL sets on same trip ???

      .d <- bind_rows(dd1, dd2)

      # if so, separate original_ind from not
      .dy <- filter(.d, original_ind == "Y")
      .dn <- filter(.d, original_ind != "Y")

      # and only keep those not original_ind = Y when the fishing_event id was missing
      .d <- bind_rows(.dy, filter(.dn, !(fishing_event_id %in% c(unique(.dy$fishing_event_id)))))

      # check if there are still duplicated fishing_event ids
      if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {
        warning(
          "Duplicate fishing_event IDs are still present despite ",
          "`remove_duplicates = TRUE`. This is potentially because of ",
          "multiple samples from the same event (), overlapping survey ",
          "stratifications. If working with the data yourself, you should ",
          "filter them after selecting specific survey stratifications. ",
          "For example, `dat <- dat[!duplicated(dat$fishing_event_id), ]`. ",
          "Tidying and plotting functions within gfplot will do this for you."
        )
      }

    } else {

      # check if there are duplicated fishing_event ids (this often true for SABLE and MSSM surveys)
      if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {
        warning(
          "Duplicate fishing_event IDs are present. This is usually because of multiple ",
          "samples from the same fishing_event, overlapping survey stratifications, ",
          "or trips that include more than one type of survey. Some cases of the ",
          "latter two case can be resolved by setting 'remove_duplicates = TRUE'. ",
          "If working with the data yourself, filter them after selecting specific ",
          "surveys. For example, `dat <- dat[!duplicated(dat$fishing_event_id), ]`. ",
          "The tidying and plotting functions within gfplot will do this for you."
        )
      }
    }
  }

  # get all fishing event info
  .fe <- read_sql("get-event-data.sql")

  # get only events from surveys that have recorded any of the species selected
  .d <- filter(.d, catch_count > 0|catch_weight > 0) # shouldn't be needed but there were some
  ssid <- unique(.d$survey_series_id)

  .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
    sql_code = .fe,
    search_flag = "-- insert ssid here", conversion_func = I
  )

  if (is.null(ssid)) {
  # something to address replicated events across different survey series
  warning(
    "Returning all survey series that recorded any of the species saught.",
    "Probably advisable to call either just one species, or just one survey type at a time.",
    "If not using default ssids, note that some survey series are nested within eachother.",
    "This can result in the duplication of fishing events in the dataframe."
  )
  }

  fe <- run_sql("GFBioSQL", .fe)

  if (!is.null(years)) {
    fe <- filter(fe, YEAR %in% years)
  }

  # if (is.null(ssid)) {
  #   fe <- filter(fe, SURVEY_SERIES_ID > 0)
  # }

  if (all(ssid %in% trawl)) {

    # uses raw fe dataframe to save time because sub event counts not need for trawl
    names(fe) <- tolower(names(fe))

    exdat <- expand.grid(fishing_event_id = unique(fe$fishing_event_id), species_code = unique(.d$species_code))

    .d <- left_join(
      exdat,
      dplyr::distinct(select(
        fe,
        #-survey_id,
        #-survey_series_id,
        -fe_parent_event_id,
        -fe_major_level_id,
        -fe_minor_level_id,
        -fe_sub_level_id,
        -hook_code,
        -lglsp_hook_count,
        -hook_desc,
        -hooksize_desc
      ))
    ) %>% left_join(.d)
  } else {

    # for other survey types, further wrangling is required
    # fe2 <- get_sub_level_counts(fe)
    fe2 <- get_skate_level_counts(fe)
    names(fe2) <- tolower(names(fe2))

    .h <- read_sql("get-ll-hook-data2.sql")

    .h <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
      sql_code = .h,
      search_flag = "-- insert ssid here", conversion_func = I
    )

    .hd <- run_sql("GFBioSQL", .h)
    names(.hd) <- tolower(names(.hd))

    fe2 <- left_join(fe2, dplyr::distinct(.hd))

    if (!any(ssid %in% trawl)) {
      exdat <- expand.grid(fishing_event_id = unique(fe2$fishing_event_id), species_code = unique(.d$species_code))

      .d <- left_join(
        exdat,
        dplyr::distinct(select(
          fe2,
          #-survey_id,
          #-survey_series_id,
          -tow_length_m,
          -mouth_width_m,
          -doorspread_m,
          -speed_mpm
        ))
      ) %>% left_join(.d)
    } else {
      exdat <- expand.grid(fishing_event_id = unique(fe2$fishing_event_id), species_code = unique(.d$species_code))
      .d <- left_join(
        exdat,
        fe2
        # dplyr::distinct(select(
        #    fe2,
        # #  -survey_id,
        # #  -survey_series_id,
        #    -fe_parent_event_id,
        #    -fe_minor_level_id
        # ))
      ) %>% left_join(.d)
    }
  }

  surveys <- get_ssids()
  names(surveys) <- tolower(names(surveys))
  .d <- inner_join(.d,
    dplyr::distinct(select(
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
    dplyr::distinct(species_df),
    by = "species_code"
  )


  # create zeros
  .d$catch_count <- ifelse(is.na(.d$catch_count), 0, .d$catch_count)
  .d$catch_weight <- ifelse(is.na(.d$catch_weight), 0, .d$catch_weight)

  # in trawl data, catch_count is only recorded for small catches
  # so 0 in the catch_count column when catch_weight > 0 seems misleading
  # note: there are also a few occasions for trawl where count > 0 and catch_weight is 0/NA
  # these lines replace false 0s with NA, but additional checks might be needed
  if (remove_false_zeros) {
    .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
    .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)
  }


  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  } else {
    u <- get_table("usability")

    names(u) <- tolower(names(u))
    .d <- left_join(
      .d,
      dplyr::distinct(select(
        u,
        usability_code,
        usability_desc
      ))
    )
  }

  if (any(ssid %in% trawl)) {
    # calculate area_swept for trawl exactly as it has been done for the density values in this dataframe
    # note: is NA if doorspread_m is missing and duration_min may be time in water (not just bottom time)

    .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
    .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
    .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)
    .d$area_swept_km2 <- .d$area_swept / 1000000
    # won't do this here because there may be ways of using mean(.d$doorspread_m) to fill in some NAs
    # .d <- dplyr::filter(.d, !is.na(area_swept))
    # instead use this to make sure false 0 aren't included
    .d$density_kgpm2 <- .d$catch_weight / .d$area_swept
    .d$density_kgpm2 <- ifelse(!is.na(.d$area_swept), .d$density_kgpm2, NA) # don't think this is doing anything
    .d$density_pcpm2 <- .d$catch_count / .d$area_swept2 # using area_swept2 is how it's done in "poc_catmat_2011"
    .d$density_pcpm2 <- ifelse(!is.na(.d$area_swept2), .d$density_pcpm2, NA) # don't think this is doing anything
  }

  if (any(ssid %in% ll)) {
    .d$hook_area_swept_km2 <- ifelse(.d$survey_series_id == 14,
      0.0054864 * 0.009144 * .d$minor_id_count,
      0.0024384 * 0.009144 * .d$minor_id_count
    )

    .d$density_ppkm2 <- .d$catch_count / (.d$hook_area_swept_km2)
    # .d$density_pppm2 <- .d$catch_count/(.d$hook_area_swept_km2*1000000)
  }

  .d <- mutate(.d,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name)
  )

  # not sure where things are getting duplicated, but this will get rid of any complete duplication
  .d <- dplyr::distinct(.d)

  species_codes <- common2codes(species)
  missing_species <- setdiff(species_codes, .d$species_code)
  if (length(missing_species) > 0) {
    warning(
      "The following species codes are not supported or do not have survey set data in GFBio.",
      paste(missing_species, collapse = ", ")
    )
  }
  add_version(as_tibble(.d))
}
