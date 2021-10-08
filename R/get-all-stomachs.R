#' Get stomach contents
#' @rdname get_stomach
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
get_survey_stomachs <- function(
 # species,
  ssid = NULL,
  # remove_bad_data = TRUE,
  unsorted_only = FALSE, usability = NULL,
  major = NULL
  ) {
  .q <- read_sql("get-stomach.sql")
  # .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
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
  # length_type <- get_spp_sample_length_type(species)
  # message(paste0("All or majority of length measurements are ", length_type))
  # search_flag <- "-- insert length type here"
  # i <- grep(search_flag, .q)
  # .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  if (unsorted_only) {
    .d <- filter(.d, sampling_desc == "UNSORTED")
  }

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  }

  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
    warning(
      "Duplicate specimen IDs are present because more than one",
      "species can be found in the same stomach."
    )
  }

  # # remove ages from unaccepted ageing methods:
  # file <- system.file("extdata", "ageing_methods.csv", package = "gfdata")
  #
  # ageing_methods <- readr::read_csv(file,
  #   col_types = readr::cols(
  #     species_code = readr::col_character()
  #   )
  # )

  # .d <- left_join(.d,
  #   select(ageing_methods, species_code, species_ageing_group),
  #   by = "species_code"
  # )

  # .d <- .d %>%
  #   mutate(
  #     age = case_when(
  #       species_ageing_group == "rockfish_flatfish_hake" & ageing_method_code %in% c(1, 3, 16, 17) ~ .d$age,
  #       species_ageing_group == "sharks_skates" & ageing_method_code %in% c(12) ~ .d$age,
  #       species_ageing_group == "dogfish" & ageing_method_code %in% c(11) ~ .d$age,
  #       species_ageing_group == "pcod_lingcod" & ageing_method_code %in% c(6) ~ .d$age,
  #       species_ageing_group == "pollock" & ageing_method_code %in% c(7) ~ .d$age,
  #       species_ageing_group == "shortraker_thornyheads" & ageing_method_code %in% c(1, 3, 4, 16, 17) ~ .d$age,
  #       is.na(species_ageing_group) ~ NA_real_
  #     )
  #   )

  # if (remove_bad_data) {
  #   .d <- .d[!(.d$length > 600 &
  #       .d$species_common_name == "north pacific spiny dogfish"), ]
  #   .d <- .d[!(.d$length > 600 & .d$species_common_name == "big skate"), ]
  #   .d <- .d[!(.d$length > 600 & .d$species_common_name == "longnose skate"), ]
  #   .d <- .d[!(.d$length > 60 & .d$species_common_name == "pacific tomcod"), ]
  #   .d <- .d[!(.d$length > 50 &
  #       .d$species_common_name == "quillback-rockfish"), ]
  #   .d <- .d[!(.d$length < 10 & .d$weight / 1000 > 1.0 &
  #       .d$species_common_name == "pacific flatnose"), ]
  # }

  as_tibble(.d)
}
