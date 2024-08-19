#' Get survey sample data with basic set information included
#'
#' @param ssid Default is to return all survey samples.
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
#' @param unsorted_only  Defaults to TRUE.
#' @param random_only Only return randomly sampled specimens.
#'   If FALSE will return all specimens collected on research trips.
#' @param include_event_info Logical for whether to append all relevant fishing event info
#'   (location, timing, effort, catch, etc.). Defaults to TRUE.
#' @param include_activity_matches Get all records collected with activity codes that match chosen ssids.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param remove_duplicates Remove duplicated specimen records due to overlapping survey
#'   stratifications when original_ind = 'N', or from known issues with MSSM trips including both survey areas.
#' @param return_dna_info Should DNA container ids and sample type be returned?
#'    This can create duplication of specimen ids for some species.  Defaults to FALSE.
#'
#' @export
#' @rdname get_data
get_all_survey_samples <- function(species, ssid = NULL,
                                   major = NULL,
                                   usability = NULL,
                                   unsorted_only = TRUE,
                                   random_only = TRUE,
                                   include_event_info = TRUE,
                                   include_activity_matches = FALSE,
                                   remove_bad_data = TRUE,
                                   remove_duplicates = FALSE,
                                   return_dna_info = FALSE) {

  .q <- read_sql("get-all-survey-samples.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)

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

    if(any(ssid %in% c(35, 41, 42, 43))){
        ssid <- unique(c(ssid, 35, 41, 42, 43))
    }

    .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
    }
  }

  if (!is.null(major)) {
    .q <- inject_filter("AND SM.MAJOR_STAT_AREA_CODE IN", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }

  search_flag <- "-- insert lengths here"
  i <- grep(search_flag, .q)

  .q[i] <- paste0("CAST(ROUND(Fork_Length/ 10, 1) AS DECIMAL(8,1)) AS Fork_Length,
                  CAST(ROUND(Standard_Length/ 10, 1) AS DECIMAL(8,1)) AS Standard_Length,
                  CAST(ROUND(Total_Length/ 10, 1) AS DECIMAL(8,1)) AS Total_Length,
                  CAST(ROUND(Second_Dorsal_Length/ 10, 1) AS DECIMAL(8,1)) AS Second_Dorsal_Length,
                  ")

  .d <- run_sql("GFBioSQL", .q)

  names(.d) <- tolower(names(.d))

  ## dna_container_id and dna_sample_type can cause duplication for some species with multiple samples collected per individual
  ## Could do something about record duplication with multiple DNA samples like combining or not returning them?
  if(!return_dna_info){
    .d <- .d |>
      select(-dna_container_id, -dna_sample_type) |>
      distinct()
  }

  # whenever ssid is included, but not activity matches
  # we need to drop duplicated records from trips that include multiple surveys
  if (!include_activity_matches & !is.null(ssid)) {
    .d <- filter(.d, (survey_series_id %in% c(ssid)))
  }

  # if using include_activity_matches = TRU`then remove_duplicates = TRUE
  if (include_activity_matches & !is.null(ssid)) {
    remove_duplicates <- TRUE
  }

  # browser()
  .d <- correct_ssids(.d, specimens = TRUE)

  # check if there are duplicate specimen ids
  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {

    if (remove_duplicates) {

    # if so, separate original_ind from not
    .dy <- filter(.d, original_ind == "Y")
    .dn <- filter(.d, original_ind != "Y"| is.na(original_ind))

    # and only keep those not original_ind = Y when the specimen id was missing
    .d <- bind_rows(.dy, filter(.dn, !(specimen_id %in% c(unique(.dy$specimen_id)))))

    # check if there are still duplicated specimen ids
    if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
      warning(
        "Duplicate specimen IDs are still present despite `remove_duplicates = TRUE`. ",
        "This is potentially because of overlapping survey stratifications, or multiple ",
        "DNA samples from the same specimen. If working with the data yourself, ",
        "you should filter them after selecting specific survey stratifications. ",
        "For example, `dat <- dat[!duplicated(dat$specimen_id), ]`. ",
        "Tidying and plotting functions within gfplot will do this for you."
      )
    }

    } else {

    # check if there are duplicated specimen ids (this often true for SABLE and MSSM surveys)
    if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
      warning(
        "Duplicate specimen IDs are present. This is usually because of multiple ",
        "DNA samples from the same specimen, overlapping survey stratifications, ",
        "or trips that include more than one type of survey. Some cases of the ",
        "latter two case can be resolved by setting 'remove_duplicates = TRUE'. ",
        "If working with the data yourself, filter them after selecting specific ",
        "surveys. For example, `dat <- dat[!duplicated(dat$specimen_id), ]`. ",
        "The tidying and plotting functions within gfplot will do this for you."
      )
    }
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

  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  ### needs testing ----

  .d$length <- NA
  .d$length_type <- NA

  species_code <- list()

  for (i in seq_along(species)) {

    length_type <- get_spp_sample_length_type(species[i])
    length_type <- tolower(length_type)

    species_code[i] <- common2codes(tolower(species[i]))

    .d[.d$species_code == tolower(species_code[i]), ]$length <- .d[.d$species_code == species_code[i], length_type]
    .d[.d$species_code == tolower(species_code[i]), ]$length_type <- length_type
  }

  ### ----

  if (unsorted_only) {
    # .d <- filter(.d, sampling_desc == "UNSORTED")
    # replaces SQL code
    # (SPECIES_CATEGORY_CODE IS NULL OR SPECIES_CATEGORY_CODE IN (1, 3, 5, 6, 7)) AND
    # (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE IN(1, 2)) AND
    .d <- filter(.d, is.na(species_category_code) | species_category_code %in% c(1))
    # # only 1 = unsorted makes sense! 3 = keepers, 5 = remains, = 6 head only, 7 doesn't exist?
    .d <- filter(.d, is.na(sample_source_code) | sample_source_code %in% c(1))
    # # only 1 = unsorted makes sense! 2 = keepers, 3 = discards
  } else {

    .ss <- get_table("Sample_Source") |> select(-ROW_VERSION)
    names(.ss) <- tolower(names(.ss))
    .d <- left_join(.d, .ss, by = "sample_source_code")

  }

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  }

  .u <- get_table("Usability") |> select(-ROW_VERSION)
  names(.u) <- tolower(names(.u))
  .d <- left_join(.d, .u, by = "usability_code")

  if (random_only) {
    # replaces SQL code
    # SM.SAMPLE_TYPE_CODE IN (1, 2, 6, 7, 8) AND
    .d <- filter(.d, sample_type_code %in% c(1, 2, 6, 7, 8)) # 8 = random from set requested by vessel master

  } else {

    ## added this to sql
    # .st <- get_table("Sample_Type") |> select(-ROW_VERSION)
    # names(.st) <- tolower(names(.st))
    # .d <- left_join(.d, .st, by = "sample_type_code")

  }

  # remove ages from unaccepted ageing methods:
  file <- system.file("extdata", "ageing_methods.csv", package = "gfdata")

  ageing_methods <- readr::read_csv(file,
    col_types = readr::cols(
      species_code = readr::col_character()
    )
  )

  .d <- left_join(.d,
    select(ageing_methods, species_code, species_ageing_group),
    by = "species_code"
  )

  .d <- .d %>%
    mutate(
      age = case_when(
        species_ageing_group == "rockfish_flatfish_hake" & ageing_method_code %in% c(1, 3, 16, 17) ~ .d$age,
        species_ageing_group == "sharks_skates" & ageing_method_code %in% c(12) ~ .d$age,
        species_ageing_group == "dogfish" & ageing_method_code %in% c(11) ~ .d$age,
        species_ageing_group == "pcod_lingcod" & ageing_method_code %in% c(6) ~ .d$age,
        species_ageing_group == "pollock" & ageing_method_code %in% c(7) ~ .d$age,
        species_ageing_group == "shortraker_thornyheads" & ageing_method_code %in% c(1, 3, 4, 16, 17) ~ .d$age,
        is.na(species_ageing_group) ~ NA_real_
      )
    )

  # removes known data problems
  # if FALSE, specimens could be reported and corrected in the database
  if (remove_bad_data) {
    .d <- .d[!(.d$length > 600 &
      .d$species_common_name == "north pacific spiny dogfish"), ]
    .d <- .d[!(.d$length > 600 & .d$species_common_name == "big skate"), ]
    .d <- .d[!(.d$length > 600 & .d$species_common_name == "longnose skate"), ]
    .d <- .d[!(.d$length > 60 & .d$species_common_name == "pacific tomcod"), ]
    .d <- .d[!(.d$length > 50 &
      .d$species_common_name == "quillback-rockfish"), ]
    .d <- .d[!(.d$length < 10 & .d$weight / 1000 > 1.0 &
      .d$species_common_name == "pacific flatnose"), ]
  }

  # dogfish were producing a whole bunch of NAs for some reason
  .d <- .d %>% filter(!is.na(specimen_id))

  if (include_event_info) {
    options(scipen=999)
    # needed for big skate because of a MSA set with an id that was getting converted

    .d <- select(.d, -minor_stat_area_code) # clashes with wrangled fe data because some sub level events have NAs here

    .f <- .d %>% filter(!is.na(fishing_event_id))
    fe_vector <- unique(.f$fishing_event_id)

    .q2 <- read_sql("get-all-survey-sets.sql")
    .q2 <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q2)
    .q2 <- inject_filter("AND FE.FISHING_EVENT_ID IN", fe_vector,
      sql_code = .q2,
      search_flag = "-- insert fe_vector here", conversion_func = I
    )

    if (!is.null(major)) {
      .q2 <- inject_filter("AND FE.MAJOR_STAT_AREA_CODE IN", major, .q2,
                          search_flag = "-- insert major here", conversion_func = I
      )
    }

    .c <- run_sql("GFBioSQL", .q2)

    names(.c) <- tolower(names(.c))
    .d <- left_join(
      .d,
      unique(select(
        .c,
        fishing_event_id,
        species_code,
        catch_weight,
        catch_count
      ))
    )

    # get all fishing event info
    .fe <- read_sql("get-event-data.sql")

    ssid_with_samples <- unique(.d$survey_series_id)

    .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid_with_samples,
                         sql_code = .fe,
                         search_flag = "-- insert ssid here", conversion_func = I
    )

    ## these don't work if we want all the sub level and minor level ids
    # .fe <- inject_filter("AND FE.FE_PARENT_EVENT_ID IS NULL OR IN", fe_vector,
    #                      sql_code = .fe,
    #                      search_flag = "-- insert fe_vector here", conversion_func = I
    # )
    #
    # .fe <- inject_filter("OR FE.FISHING_EVENT_ID IN", fe_vector,
    #                      sql_code = .fe,
    #                      search_flag = "-- insert fe_vector here", conversion_func = I
    # )

    # so reduce data size using trip ids instead?
    trip_vector <- unique(.d$trip_id)

    .fe <- inject_filter("AND FE.TRIP_ID IN", trip_vector,
                         sql_code = .fe,
                         search_flag = "-- insert fe_vector here", conversion_func = I
    )


    if (!is.null(major)) {
      .fe <- inject_filter("AND FE.MAJOR_STAT_AREA_CODE IN", major, .fe,
                           search_flag = "-- insert major here", conversion_func = I
      )
    }

    fe <- run_sql("GFBioSQL", .fe)

  if(any(!is.na(fe$FE_SUB_LEVEL_ID))) {
     if(any(fe$FE_SUB_LEVEL_ID > 1)) {

       fe2 <- get_skate_level_counts(fe)

       if (sum(!is.na(unique(fe2$HOOK_CODE))) < 2 | sum(!is.na(unique(fe2$HOOKSIZE_DESC))) < 2) {
       fe2 <- get_parent_level_counts(fe)
       }
     }
     fe2 <- get_parent_level_counts(fe)
     } else {
     fe2 <- get_parent_level_counts(fe)
     }

    fe2 <- fe2 %>% select(-REASON_DESC, -USABILITY_CODE,
                           -GROUPING_CODE_ORIGINAL, -GROUPING_DESC_ORIGINAL,
                           -GROUPING_CODE, -ORIGINAL_IND) # avoid clashing with values for samples

    names(fe2) <- tolower(names(fe2))

    .d <- left_join(.d, fe2)

    # in trawl data, catch_count is only recorded for small catches
    # so 0 in the catch_count column when catch_weight > 0 seems misleading
    # note: there are also a few occasions for trawl where count > 0 and catch_weight is 0/NA
    # these lines replace false 0s with NA, but additional checks might be needed

    .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
    .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)


    .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
    .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
    .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)
    .d$area_swept_km2 <- .d$area_swept / 1000000
    .d$hook_area_swept_km2 <- ifelse(.d$survey_series_id == 14,
      0.0054864 * 0.009144 * .d$minor_id_count,
      0.0024384 * 0.009144 * .d$minor_id_count
    )

    .d <- .d %>% select(where(~ !all(is.na(.x))))
    .d <- unique(.d)
  }

  add_version(as_tibble(.d))
}
