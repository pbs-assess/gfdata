#' Get all data
#'
#' These functions get all survey set or sample data for a set of species by
#' major area, activity, or specific surveys. The main functions in this package
#' focus on retrieving the more commonly used typs of data and are often limited
#' to sets and samples that conform to current design-based standards and survey
#' grids. These functions will retrieve everything and therefore require careful
#' consideration of what data types are reasonable to include depending on the
#' purpose. For this reason these function return a lot of columns, although the
#' exact number depends on which types of surveys are being returned.
#'
#' @param ssid Default is to return all data from all surveys. The most
#'   comprehensive and useful ids include: contemporary trawl (1, 3, 4, 16),
#'   historic trawl (2), IPHC (14), sablefish (35), and HBLL (22, 36, 39, 40).
#' @param years Default is NULL, which returns all years.
#' @param major Character string (or vector) of major stat area code(s) to
#'   include (characters). Use get_major_areas() to lookup area codes with
#'   descriptions. Default is NULL.
#' @param join_sample_ids This option was problematic, so now reverts to FALSE.
#' @param remove_false_zeros If `TRUE` will make sure weights > 0 don't have
#'   associated counts of 0 and vice versa. Mostly useful for trawl data where
#'   counts are only taken for small catches.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'   length or weight values and duplications due to trips that include multiple
#'   surveys. Default is TRUE.
#' @param remove_duplicates Logical for whether to remove duplicated event
#'   records due to overlapping survey stratifications when original_ind = 'N',
#'   or from known issues with MSSM trips including both survey areas.
#'   Defaults to FALSE when ssids are supplied and activity matches aren't
#'   included. Otherwise turns on automatically.
#' @param include_activity_matches Get all surveys with activity codes that
#'   match chosen ssids.
#' @param usability A vector of usability codes to include. Defaults to NULL,
#'   but typical set for trawl is`c(0, 1, 2, 6)`. IPHC codes may be different to
#'   other surveys and the modern Sablefish survey doesn't seem to assign
#'   usabilities.
#' @param grouping_only Defaults to FALSE, which will return all specimens or sets
#'   collected on research trips. TRUE returns only sets or specimens from fishing
#'   events with grouping codes that match that expected for a survey. Can also be
#'   achieved by filtering for specimens where `!is.na(grouping_code)`.
#' @param quiet_option Default option, `"message"`, suppresses messages from
#'   sections of code with lots of `join_by` messages. Any other string will allow
#'   messages.
#'
#' @export
#' @rdname get_all
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
                                ssid = NULL,
                                major = NULL,
                                years = NULL,
                                join_sample_ids = FALSE,
                                remove_false_zeros = TRUE,
                                remove_bad_data = TRUE,
                                remove_duplicates = FALSE,
                                include_activity_matches = FALSE,
                                usability = NULL,
                                grouping_only = FALSE,
                                quiet_option = "message"
                                ) {
  .q <- read_sql("get-all-survey-sets.sql")

  if (!is.null(species)) {
    .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  }


  if (!is.null(ssid)) {

    ssid_original <- ssid

    if(any(ssid %in% c(35, 41, 42, 43))){
      ssid <- unique(c(ssid, 35, 41, 42, 43))
    }

    if(any(ssid %in% c(6,7,67))){
      ssid <- unique(c(ssid, 6,7,67))
    }

    if (include_activity_matches) {
      ## draft approach that gets all samples collected using the same activities as the ssid(s) of interest
      .a <- read_sql("get-activity-code.sql")
      .a <- run_sql("GFBioSQL", .a)

      .a <- filter(.a, SURVEY_SERIES_ID %in% ssid) |> distinct()

      activities <- unique(.a$ACTIVITY_CODE)
      .q <- inject_filter("AND TA.ACTIVITY_CODE IN", activities,
        sql_code = .q,
        search_flag = "-- insert ssid here", conversion_func = I
      )
    } else {
      .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
        sql_code = .q,
        search_flag = "-- insert ssid here", conversion_func = I
      )
    }
  } else {
    remove_duplicates <- TRUE
  }

  if (!is.null(major)) {
    .q <- inject_filter("AND FE.MAJOR_STAT_AREA_CODE IN", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }

  d0 <- .d <- run_sql("GFBioSQL", .q)

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
      "sample data, it is safer to use include_event_info = TRUE ",
      "in get_all_survey_samples() instead."
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

    if(is.null(ssid)&is.null(major)){
      stop(paste0("No survey set data for ", toString(species), "."))
    } else {
      if(!is.null(ssid)&is.null(major)){
        stop(paste0("No survey set data for ", toString(species), " from ssid(s) ", toString(ssid), "."))
      }
      if(is.null(ssid)&!is.null(major)){
        stop(paste0("No survey set data for ", toString(species), " from major area(s) ", toString(major), "."))
      }
      if(!is.null(ssid)&!is.null(major)){
        stop(paste0("No survey set data for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
      }
    }
  }

  names(.d) <- tolower(names(.d))

  # whenever ssid is included, but not activity matches
  # we need to drop duplicated records from trips that include multiple surveys
  if (!include_activity_matches & !is.null(ssid)) {
    .d <- filter(.d, (survey_series_id %in% c(ssid)))
  }

  # if using include_activity_matches = TRUE then remove_duplicates = TRUE
  if (include_activity_matches & !is.null(ssid)) {
    remove_duplicates <- TRUE
  }

  # get all fishing event info
  .fe <- read_sql("get-event-data.sql")

  # get only events from surveys that have recorded any of the species selected
  .d <- filter(.d, catch_count > 0 | catch_weight > 0) # shouldn't be needed but there were some
  ssid_with_catch <- unique(.d$survey_series_id)

  # browser()

  #d1 <- .d #<- select(.d, -survey_series_id)

  .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid_with_catch,
    sql_code = .fe,
    search_flag = "-- insert ssid here", conversion_func = I
  )

  if (!is.null(major)) {
    .fe <- inject_filter("AND FE.MAJOR_STAT_AREA_CODE IN", major, .fe,
      search_flag = "-- insert major here", conversion_func = I
    )
  }

  fe <- run_sql("GFBioSQL", .fe)



  fe <- fe |> distinct() |> # not sure why, but seems to be some complete duplication
    filter(FE_MAJOR_LEVEL_ID < 700| is.na(FE_MAJOR_LEVEL_ID)) # removes CTD drops

  if (!is.null(years)) {
    fe <- filter(fe, YEAR %in% years)
  }

  # if (is.null(ssid)) {
  #   fe <- filter(fe, SURVEY_SERIES_ID > 0)
  # }

  suppressMessages(

  if (all(ssid_with_catch %in% trawl)) {
    # uses raw fe dataframe to save time because sub event counts not need for trawl
    names(fe) <- tolower(names(fe))

    .d <- expand.grid(
      fishing_event_id = unique(fe$fishing_event_id),
      species_code = unique(.d$species_code)
    ) |>
      left_join(dplyr::distinct(select(
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
      ))) %>%
      left_join(.d)

  } else {
    # for other survey types, further wrangling is required
    # TODO: might be improved by making trap surveys a special case but for now this works ok
    # TODO: could split by survey to see when skate level is needed rather than applying to all
    # start by checking the skate level counts and gear details
    # sk <- get_skate_level_counts(fe)
    # names(sk) <- tolower(names(sk))

    # get catch for sub levels if skate counts > 1 and gear differs between skates
    # sks <- sk %>% filter(skate_count > 1)
    # fe_vector <- unique(sks$fishing_event_id)

    spp_codes <- unique(.d$species_code)

    fe1 <- get_skate_level_counts(fe)

    # browser()

    if(any(fe$FE_SUB_LEVEL_ID > 1) & (sum(!is.na(unique(fe1$HOOK_CODE))) > 1 | sum(!is.na(unique(fe1$HOOKSIZE_DESC))) > 1)) {

        .h <- read_sql("get-ll-sub-level-hook-data.sql")

        .h <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid_with_catch,
          sql_code = .h,
          search_flag = "-- insert ssid here", conversion_func = I
        )

        .hd <- run_sql("GFBioSQL", .h)
        .hd <- dplyr::distinct(.hd) #%>% select(-FE.FISHING_EVENT_ID)
        names(.hd) <- tolower(names(.hd))
        names(fe1) <- tolower(names(fe1))

        fe2 <- left_join(fe1, .hd) |>
          # select(-survey_series_id) |>
          distinct()

        .d <- expand.grid(
          fishing_event_id = unique(fe2$fishing_event_id),
          species_code = unique(.d$species_code)
        ) |>
          left_join(fe2) |>
          left_join(.d)


        slc_list <- list()
        for (i in seq_along(spp_codes)) {
          .slc <- read_sql("get-sub-level-catch.sql")
          .slc <- inject_filter("", spp_codes[i], sql_code = .slc)
          .slc <- inject_filter("AND C.SPECIES_CODE IN", spp_codes[i],
            sql_code = .slc,
            search_flag = "-- insert species again here"
          )
          ## this didn't work, not sure why, but suspect it isn't working for get-all-survey-sets.sql either
          # .slc <- inject_filter("AND FE.FE_PARENT_EVENT_ID IN", fe_vector,
          #                      sql_code = .slc,
          #                      search_flag = "-- insert fe_vector here", conversion_func = I
          # )
          slc_list[[i]] <- run_sql("GFBioSQL", .slc)
        }
        slc <- do.call(rbind, slc_list)
        names(slc) <- tolower(names(slc))

        .d1 <- .d %>% filter(!(skate_count > 1) | is.na(skate_count))

        .d2 <- .d %>%
          filter(skate_count > 1) |>
          select(-catch_count) |>
          left_join(slc)


        .d <- bind_rows(.d1, .d2)
      } else {

    ## if hooks do not differ between skates, get hook data and catch for whole event

    .h <- read_sql("get-ll-hook-data2.sql")

    .h <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid_with_catch,
      sql_code = .h,
      search_flag = "-- insert ssid here", conversion_func = I
    )

    .hd <- run_sql("GFBioSQL", .h)
    names(.hd) <- tolower(names(.hd))

    .hd <- dplyr::distinct(.hd)

    ## old sub_level function moved from utils.R
    fe3 <- get_parent_level_counts(fe)
    names(fe3) <- tolower(names(fe3))


    fe3 <- left_join(fe3, .hd) |>
      # select(-survey_series_id) |>
      dplyr::distinct()

    .d <- expand.grid(
      fishing_event_id = unique(fe3$fishing_event_id),
      species_code = unique(.d$species_code)
    ) |>
      left_join(fe3) |>
      left_join(.d)

  }
  }
  , classes = quiet_option)


  suppressMessages(
  if (remove_bad_data) {
      .d <- correct_ssids(.d)
  }
  , classes = quiet_option)

  if (!is.null(ssid)){

    .d <- .d |> group_by(fishing_event_id, survey_series_id) |>
      mutate(grouping_desc = ifelse(is.logical(na.omit(grouping_desc)), NA, na.omit(grouping_desc)),
             grouping_code = mean(grouping_code, na.rm = TRUE),
             grouping_code = ifelse(is.nan(grouping_code), NA, grouping_code)
      ) |> dplyr::distinct() |> ungroup()

    if(any(ssid %in% c(6,7,67))&!include_activity_matches){
      ssid <- ssid_original
    }

    .d <- filter(.d, survey_series_id %in% ssid)

    if (is.null(major)) {
      print(
        paste0("Returning all sets belonging to survey series ", toString(ssid), " when ", toString(species), " were recorded.")
      )
    }
    if (!is.null(major)) {
      print(
        paste0("Returning sets within major area(s) ", toString(major), " and belonging to survey series ", toString(ssid), " when ", toString(species), " were recorded.")
      )
    }


  } else {

      .d <- .d |> group_by(fishing_event_id) |>
        mutate(grouping_desc = ifelse(grouping_code == grouping_code_original, grouping_desc, NA),
               grouping_desc = ifelse(is.logical(na.omit(grouping_desc)), NA, na.omit(grouping_desc)),
               grouping_code = ifelse(grouping_code == grouping_code_original, grouping_code, NA),
               grouping_code = mean(grouping_code, na.rm = TRUE),
               grouping_code = ifelse(is.nan(grouping_code), NA, grouping_code)
               ) |> dplyr::distinct() |> ungroup()

    if (is.null(major)) {
      print(
        paste0("Returning all survey series that recorded any ", toString(species), ".")
      )
    }
    if (!is.null(major)) {
      print(
        paste0("Returning all survey series that recorded any ", toString(species), " from major area(s) ", toString(major), ".")
      )
    }
  }

  # check if there are duplicate fishing_event ids
  if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {


    if (remove_duplicates) {

      # if so, separate original_ind from not
      .dy <- filter(.d, original_ind == "Y")
      .dn <- filter(.d, original_ind != "Y" | is.na(original_ind))

      # and only keep those not original_ind = Y when the fishing_event id was missing
      .d <- bind_rows(.dy, filter(.dn, !(fishing_event_id %in% c(unique(.dy$fishing_event_id)))))

      # check if there are still duplicated fishing_event ids
      if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {
        warning(
          "Duplicate fishing_event_ids are still present despite ",
          "`remove_duplicates = TRUE`. This may be because of overlapping ",
          "survey stratifications or multiple skates per event ",
          "(specifically when at least one survey included used skates with ",
          "differences in gear type), but could also be due to trips participating ",
          "in more than one type of survey. If the latter, location, gear, or `reason_desc` ",
          "columns should be used to choose which events to keep. ",
          "After selecting specific survey stratifications and determining that ",
          "all relevant variables are accurate, the remaining duplications ",
          "can be filtered using `dat <- dat[!duplicated(dat$fishing_event_id), ]`. "
        )
      }
    } else {
      # check if there are duplicated fishing_event ids (this often true for SABLE and MSSM surveys)
      if (length(.d$fishing_event_id) > length(unique(.d$fishing_event_id))) {
        warning(
          "Duplicate fishing_event_ids are present. This is usually because of ",
          "overlapping survey stratifications, multiple skates per event ",
          "(specifically when at least one survey included used skates with ",
          "differences in gear type), or trips that include more than one type of ",
          "survey. Some cases of the former can be resolved by setting ",
          "'remove_duplicates = TRUE'. If the latter, location, gear, or `reason_desc` ",
          "columns should be used to choose which events to keep. ",
          "After selecting specific survey stratifications and determining that ",
          "all relevant variables are accurate, the remaining duplications ",
          "can be filtered using `dat <- dat[!duplicated(dat$fishing_event_id), ]`. "
        )
      }
    }
  }

  if(nrow(.d[.d$survey_series_id %in% c(35, 41, 42, 43),])> 0){
    warning("All sablefish research related sets are returned as survey_series_id 35. ",
            "To separate types of sets, use reason_desc and grouping_code variables.")
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
      )),
      by = "usability_code"
    )
  }

  if (any(ssid_with_catch %in% trawl)) {
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

  if (any(ssid_with_catch %in% ll)) {
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

  if(grouping_only){
    .d <- filter(.d, !is.na(grouping_code))

    if (nrow(.d) < 1) {

      if(is.null(ssid)&is.null(major)){
        stop(paste0("No survey set data with expected grouping codes for ", toString(species), "."))
      } else {
        if(!is.null(ssid)&is.null(major)){
          stop(paste0("No survey set data with expected grouping codes for ", toString(species), " from ssid(s) ", toString(ssid), "."))
        }
        if(is.null(ssid)&!is.null(major)){
          stop(paste0("No survey set data with expected grouping codes for ", toString(species), " from major area(s) ", toString(major), "."))
        }
        if(!is.null(ssid)&!is.null(major)){
          stop(paste0("No survey set data with expected grouping codes for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
        }
      }
    }
  }

  # return only events from surveys that have recorded any of the species selected
  # rechecking this after SSID corrections
  .dpos <- filter(.d, catch_count > 0 | catch_weight > 0)
  ssid_with_catch <- unique(.dpos$survey_series_id)
  .d <- filter(.d, survey_series_id %in% ssid_with_catch)


  .d <- .d |> relocate(species_common_name, catch_count, catch_weight, survey_series_id, survey_abbrev, year, fishing_event_id) |>
    arrange(species_common_name, survey_series_id, -year, -fishing_event_id)

  # not sure where things are getting duplicated, but this will get rid of any complete duplication
  .d <- dplyr::distinct(.d)

  # this drops any columns entirely populated with NAs
  .d <- .d %>% select(where(~ !all(is.na(.x))))

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
