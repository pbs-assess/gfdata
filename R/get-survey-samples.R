#' Get survey sample data with basic set information included
#'
#' @param ssid Default is to return all survey samples.
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
#' @param random_only Only return randomly sampled specimens.
#'   If FALSE will return all specimens collected on research trips.
#' @param include_activity_matches Get all records collected with activity codes that match chosen ssids.
#' @param include_event_info Logical for whether to append all relevant fishing event info
#'   (location, timing, effort, catch, etc.). Defaults to false.
#' @param unsorted_only  Defaults to TRUE.
#' @param return_dna_info Should DNA container ids and sample type be returned?
#'    This can create duplication of specimen ids for some species.  Defaults to FALSE.
#'
#' @export
#' @rdname get_data
get_survey_samples2 <- function(species, ssid = NULL,
                                remove_bad_data = TRUE,
                                unsorted_only = TRUE,
                                usability = NULL,
                                random_only = TRUE,
                                include_activity_matches = FALSE,
                                include_event_info = FALSE,
                                return_dna_info = FALSE,
                                major = NULL) {

  .q <- read_sql("get-survey-samples2.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)

  if (!is.null(ssid)) {
    if (include_activity_matches){
        ## draft approach that gets all samples collected using the same activities as the ssid(s) of interest
        .a <- read_sql("get-activity-code.sql")

        .a <-  filter(.a, SURVEY_SERIES_ID %in% ssid)

        activities <- unique(.a$ACTIVITY_CODE)
        .q <- inject_filter("AND TA.ACTIVITY_CODE IN", activities,
                            sql_code = .q,
                            search_flag = "-- insert ssid here", conversion_func = I
        )
    } else {
    .q <- inject_filter("AND SS.SURVEY_SERIES_ID IN", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
    }
  }

  if (!is.null(major)) {
    .q <- inject_filter("AND SM.MAJOR_STAT_AREA_CODE =", major, .q,
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

  ## if wanting a different behavior with activity matches
  # if (!include_activity_matches & !is.null(ssid)) {
  # .d <- filter(.d, (SURVEY_SERIES_ID %in% c(ssid, NA)))
  # }

  surveys <- get_ssids()

  .d <- inner_join(.d,
    unique(select(
      surveys,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC,
      SURVEY_ABBREV
    )),
    by = "SURVEY_SERIES_ID"
  )

  names(.d) <- tolower(names(.d))

  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  ### needs testing ----

  .d$length <- NA
  .d$length_type <- NA

  species_code <- list()

  for (i in seq_along(species)) {

    length_type <- get_spp_sample_length_type(species[i])
    length_type <- tolower(length_type)

    #browser()

    species_code[i] <- common2codes(tolower(species[i]))

    .d[.d$species_code == tolower(species_code[i]), ]$length <- .d[.d$species_code == species_code[i], length_type]
    .d[.d$species_code == tolower(species_code[i]), ]$length_type <- length_type
  }

  # .d <- .d %>% mutate(length_type = length_type)
  ### ----

  if (unsorted_only) {
    .d <- filter(.d, sampling_desc == "UNSORTED")
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
    # (SPECIES_CATEGORY_CODE IS NULL OR SPECIES_CATEGORY_CODE IN (1, 3, 5, 6, 7)) AND
    # (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE IN(1, 2)) AND

    .d <- filter(.d, sample_type_code %in% c(1, 2, 6, 7, 8))
    #.d <- filter(.d, is.na(species_category_code) | species_category_code %in% c(1, 3, 5, 6, 7)) # only 1 makes sense!
    #.d <- filter(.d, is.na(sample_source_code) | sample_source_code %in% c(1,2)) # only 1 makes sense!
  }

  ## dna_container_id and dna_sample_type can cause duplication for some species with multiple samples collected per individual
  ## Could do something about record duplication with multiple DNA samples like combining or not returning them?
  if(!return_dna_info){
    .d <- .d |>
      select(-dna_container_id, -dna_sample_type) |>
      distinct()
  }

  # check if there are duplicate specimen ids
  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {

    # if so, separate original_ind from not
    .dy <- filter(.d, original_ind == "Y")
    .dn <- filter(.d, original_ind != "Y")

    # and only keep those not original_ind = Y when the specimen id was missing
    .d <- bind_rows(.dy, filter(.dn, !(specimen_id %in% c(unique(.dy$specimen_id)))))

    # check if there is still duplication (this seems to be true for SABLE and MSSM surveys)
    if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
    warning(
      "Duplicate specimen IDs are present because of overlapping survey ",
      "stratifications, or multiple DNA samples from the same specimen. ",
      "If working with the data yourself, filter them after selecting specific ",
      "surveys. For example, `dat <- dat[!duplicated(dat$specimen_id), ]`. ",
      "The tidying and plotting functions within gfplot will do this for you."
    )
    }
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

    .f <- .d %>% filter(!is.na(fishing_event_id))
    fe_vector <- unique(.f$fishing_event_id)

    .q2 <- read_sql("get-survey-sets.sql")
    .q2 <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q2)
    .q2 <- inject_filter("AND FE.FISHING_EVENT_ID IN", fe_vector,
      sql_code = .q2,
      search_flag = "-- insert fe vector here", conversion_func = I
    )

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

    ssid <- unique(.d$survey_series_id)
    .fe <- inject_filter("AND SS.SURVEY_SERIES_ID IN", ssid,
      sql_code = .fe,
      search_flag = "-- insert ssid here", conversion_func = I
    )

    fe <- run_sql("GFBioSQL", .fe) %>% select(-USABILITY_CODE, -GROUPING_CODE) # avoid classing with values for samples

    fe2 <- get_sub_level_counts(fe)
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

    .d <- unique(.d)
  }


  add_version(as_tibble(.d))
}
