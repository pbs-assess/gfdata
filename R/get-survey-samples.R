#' Get survey sample data with basic set information included
#'
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
#'
#' @export
#' @rdname get_data
get_survey_samples2 <- function(species, ssid = NULL,
                               remove_bad_data = TRUE, unsorted_only = TRUE,
                               usability = NULL, major = NULL) {
  .q <- read_sql("get-survey-samples.sql")
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

  add_version(as_tibble(.d))
}

#' @export
#' @param unsorted_only Remove sorted biological data ('keepers' and 'discards'
#'  and unknown). Default = TRUE.
#' @param return_all_lengths Include all length types, rather than just
#'  with most common measurement. Default = FALSE.
#' @rdname get_data
get_commercial_samples <- function(species, unsorted_only = TRUE,
                                   return_all_lengths = FALSE,
                                   major = NULL,
                                   usability = NULL) {
  .q <- read_sql("get-comm-samples.sql")
  .q <- inject_filter("AND SM.SPECIES_CODE IN", species, sql_code = .q)

  length_type <- get_spp_sample_length_type(species)
  message(paste0("All or majority of length measurements are ", length_type))
  search_flag <- "-- insert length type here"
  i <- grep(search_flag, .q)

  if (return_all_lengths){
    .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,
                    CAST(ROUND(Fork_Length/ 10, 1) AS DECIMAL(8,1)) AS Fork_Length,
                    CAST(ROUND(Standard_Length/ 10, 1) AS DECIMAL(8,1)) AS Standard_Length,
                    CAST(ROUND(Total_Length/ 10, 1) AS DECIMAL(8,1)) AS Total_Length,
                    CAST(ROUND(Second_Dorsal_Length/ 10, 1) AS DECIMAL(8,1)) AS Second_Dorsal_Length,
                    ")
  } else {
    .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")
  }

  if (!is.null(major)) {
    .q <- inject_filter("AND SM.MAJOR_STAT_AREA_CODE =", major, .q,
                        search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)
  .d <- mutate(.d, year = lubridate::year(trip_start_date))

  if (!return_all_lengths){
    .d <- .d %>% mutate(length_type = length_type)
  } else {
    .n <- .d %>% select(-fork_length,-standard_length,-total_length, -second_dorsal_length)
    .n <- names(.n)
    .d <- .d %>% select(-length) %>%
      tidyr::pivot_longer(
        cols = tidyr::ends_with("_length"),
        names_to = "length_type",
        values_to = "length",
        values_drop_na = TRUE
      ) %>% dplyr::relocate(tidyr::all_of(.n))
  }

  duplicate_specimen_ids <- sum(duplicated(.d$specimen_id))
  if (duplicate_specimen_ids > 0) {
    warning(
      duplicate_specimen_ids, " duplicate specimen IDs detected for",
      species, " . Removing them."
    )
    .d <- .d[!duplicated(.d$specimen_id), , drop = FALSE]
  }
  # assertthat::assert_that(sum(duplicated(.d$specimen_id)) == 0)

  if (unsorted_only) {
    .d <- filter(.d, sampling_desc == "UNSORTED")
  }

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  }

  # remove ages from unaccepted ageing methods:
  file <- system.file("extdata", "ageing_methods.csv",
                      package = "gfdata"
  )

  ageing_methods <- readr::read_csv(file,
                                    col_types = readr::cols(
                                      species_code = readr::col_character()
                                    )
  )

  .d <- left_join(.d,
                  select(ageing_methods, species_code, .data$species_ageing_group),
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

  add_version(as_tibble(.d))
}

#' @export
#' @rdname get_data
get_catch <- function(species, major = NULL) {
  .q <- read_sql("get-catch.sql")
  if (!is.null(species)) {
    .q <- inject_filter("WHERE SP.SPECIES_CODE IN", species, sql_code = .q)
  }
  if (!is.null(major)) {
    .q <- inject_filter("AND MC.MAJOR_STAT_AREA_CODE =", major, .q,
                        search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  .d$year <- lubridate::year(.d$best_date)
  add_version(as_tibble(.d))
}

# #' For a given species, extracts all species co-caught with the given species
# #' from 2008 (with minimum catch of 100 kg of each species). Output includes
# #' the number of fishing events in which each species was co-caught, the number
# #' of years from 2008 in which each species was co-caught in at least one
# #' fishing event, and the sum of landed kg for each species over all of the
# #' fishing events it was caught with the given species.
# #'
# #' @export
# #' @param fishery_sector Name of fishery sector to filter on (optional). Will
# #'  be converted to uppercase. Run [get_fishery_sectors()] for a look-up table of
# #'  available fishery sectors to select from.
# #' @param gear Name of gear type to filter on (optional). Will be converted to
# #'  uppercase. Run [get_comm_gear_types()] for a look-up table of available gear
# #'  types to select from.
# #' @examples
# #' \dontrun{
# #' rex_cocaught <- get_cocaught_species(610, "groundfish trawl")
# #' }
# get_cocaught_species <- function(species, fishery_sector = NULL, gear = NULL,
#   target_min = 10, cocaught_min = 10) {
#   .q <- read_sql("get-cocaught-species.sql")
#   .q <- inject_filter("WHERE MC.SPECIES_CODE IN", species, sql_code = .q)
#   .q <- inject_filter("AND LANDED_KG >= ", target_min, .q,
#     search_flag = "-- insert target_min here", conversion_func = I)
#   .q <- inject_filter("AND LANDED_KG >= ", target_min, .q,
#     search_flag = "-- insert cocaught_min here", conversion_func = I)
#   if (!is.null(fishery_sector)) {
#     .q <- inject_filter("AND FISHERY_SECTOR IN", toupper(fishery_sector), sql_code = .q,
#       search_flag = "-- insert fishery here", conversion_func = I)
#   }
#   if (!is.null(gear)) {
#     .q <- inject_filter("AND GEAR IN", toupper(gear), .q,
#       search_flag = "-- insert gear here", conversion_func = I
#     )
#   }
#   .d <- run_sql("GFFOS", .q)
#   .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
#     toupper("north pacific spiny dogfish") # to match GFBioSQL
#   names(.d) <- tolower(names(.d))
#   .d$species_common_name <- tolower(.d$species_common_name)
#   as_tibble(.d)
# }
