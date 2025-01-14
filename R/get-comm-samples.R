#' Similar to main version but with option to return event info
#' @export
#'
#' @param unsorted_only Remove sorted biological data ('keepers' and 'discards'
#'  and unknown). Default = TRUE.
#' @param return_all_lengths Include all length types, rather than just
#'  with most common measurement. Default = FALSE.
#' @param include_event_info Logical for whether to append all relevant fishing event info
#'  (location, timing, effort, catch, etc.). Default = FALSE.
#' @param return_dna_info  Should DNA container ids and sample type be returned?
#'  This can create duplication of specimen ids for some species. Default = FALSE.
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param usability A vector of usability codes to include. Defaults to all.
#'  IPHC codes may be different to other surveys.
#'
#' @rdname get_data
get_commercial_samples2 <- function(species,
                                    unsorted_only = TRUE,
                                    return_all_lengths = FALSE,
                                    include_event_info = FALSE,
                                    return_dna_info = FALSE,
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

  .other_wts <- .d[!duplicated(.d$specimen_id), , drop = FALSE] %>% # removes duplicates
    select(gutted, jcut, glazed_jcut) # keep only alternate weight types

  dw <- colSums(!is.na(.other_wts)) # tally each type

  if(max(dw == 0)){
    .d$weight_other <- NA
    .d$weight_other_type <- NA
  } else {
    .d$weight_other <- .d[names(dw[dw == max(dw)])]
    .d$weight_other_type <- names(dw[dw == max(dw)])
  }

  ## dna_container_id and dna_sample_type can cause duplication for some species with multiple samples collected per individual
  ## Could do something about record duplication with multiple DNA samples like combining or not returning them?
  if(!return_dna_info){
    .d <- .d |>
      select(-dna_container_id, -dna_sample_type) |>
      distinct()
  }

  duplicate_specimen_ids <- sum(duplicated(.d$specimen_id))
  # if (duplicate_specimen_ids > 0) {
  #   warning(
  #     duplicate_specimen_ids, " duplicate specimen IDs detected for",
  #     species, " . Removing them."
  #   )
  #   .d <- .d[!duplicated(.d$specimen_id), , drop = FALSE]
  # }

  if (duplicate_specimen_ids > 0) {
    warning(
      duplicate_specimen_ids, "duplicate specimen IDs are present for ", species, ". ",
      "This may be because of multiple DNA samples from some specimens. ",
      "If working with the data yourself, they can be filtered with ",
      "`dat <- dat[!duplicated(dat$specimen_id), ]`. ",
      "The tidying and plotting functions within gfplot may do this for you."
    )
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

  if(include_event_info){

    fe_vector <- unique(.d$fishing_event_id)

    # get all fishing event info
    .fe <- read_sql("get-event-data.sql")
    fe <- run_sql("GFBioSQL", .fe) %>% select(-USABILITY_CODE, -GROUPING_CODE) # avoid classing with values for samples

    fe2 <- get_sub_level_counts(fe)
    names(fe2) <- tolower(names(fe2))
    .d <- left_join(.d, unique(select(fe2,
                                      -time_deployed, -time_retrieved,
                          -latitude,
                          -longitude,
                          -major_stat_area_code,
                          -minor_stat_area_code,
                          -vessel_id)))

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

