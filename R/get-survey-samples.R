#' Get survey sample data with basic set information included
#'
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
#' @param include_event_info Logical for whether to append all relevant fishing event info
#'   (location, timing, effort, catch, etc.). Defaults to false.
#'
#' @export
#' @rdname get_data
get_survey_samples2 <- function(species, ssid = NULL,
                               remove_bad_data = TRUE, unsorted_only = TRUE,
                               usability = NULL,
                               include_event_info = FALSE,
                               major = NULL) {
  .q <- read_sql("get-survey-samples2.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)

  if (!is.null(ssid)) {
    .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
                        sql_code = .q,
                        search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  if (!is.null(major)) {
    .q <- inject_filter("AND SM.MAJOR_STAT_AREA_CODE =", major, .q,
                        search_flag = "-- insert major here", conversion_func = I
    )
  }
  length_type <- get_spp_sample_length_type(species)

  message(paste0("All or majority of length measurements are ", length_type))
  search_flag <- "-- insert length type here"
  i <- grep(search_flag, .q)

  .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")

  .d <- run_sql("GFBioSQL", .q)


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

  .d <- .d %>% mutate(length_type = length_type)

  if (unsorted_only) {
    .d <- filter(.d, sampling_desc == "UNSORTED")
  }

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  }

  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
    warning(
      "Duplicate specimen IDs are present because of overlapping survey ",
      "stratifications. If working with the data yourelf, filter them after ",
      "selecting specific surveys. For example, ",
      "`dat <- dat[!duplicated(dat$specimen_id), ]`. ",
      "The tidying and plotting functions within gfplot will do this for you."
    )
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



  if(include_event_info){

  fe_vector <- unique(.d$fishing_event_id)

  .q2 <- read_sql("get-survey-sets.sql")
  .q2 <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q2)
  .q2 <- inject_filter("AND FE.FISHING_EVENT_ID IN", fe_vector,
                        sql_code = .q2,
                        search_flag = "-- insert fe vector here", conversion_func = I
    )

  .c <- run_sql("GFBioSQL", .q2)


  names(.c) <- tolower(names(.c))
  .d <- left_join(.d,
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
  .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
                       sql_code = .fe,
                       search_flag = "-- insert ssid here", conversion_func = I
  )

  fe <- run_sql("GFBioSQL", .fe) %>% select(-USABILITY_CODE, -GROUPING_CODE) # avoid classing with values for samples

  fe2 <- get_sub_level_counts(fe)
  names(fe2) <- tolower(names(fe2))

  .d <- left_join(.d, fe2)

  .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
  .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
  .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)
  .d$area_swept_km2 <- .d$area_swept/1000000
  .d$hook_area_swept_km2 <- ifelse(.d$survey_series_id == 14,
                                   0.0054864 * 0.009144 * .d$minor_id_count,
                                   0.0024384 * 0.009144 * .d$minor_id_count)

  .d <- unique(.d)
  }


  add_version(as_tibble(.d))

}
