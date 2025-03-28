#'
#' @param species One or more species common names (e.g. "pacific ocean
#'   perch") or one or more species codes (e.g. `396`). Species codes can be
#'   specified as numeric vectors `c(396, 442`) or characters `c("396", "442")`.
#'   Numeric values shorter than 3 digits will be expanded to 3 digits and
#'   converted to character objects (`1` turns into `"001"`). Species common
#'   names and species codes should not be mixed. If any element is missing a
#'   species code, then all elements will be assumed to be species common
#'   names. Does not work with non-numeric species codes, so in those cases the
#'   common name will be needed.
#' @param ssid A numeric vector of survey series IDs. Run [get_ssids()] for a
#'   look-up table of available survey series IDs with surveys series
#'   descriptions. Default is to return all data from all surveys. Some of the
#'   most useful ids include: contemporary trawl (1, 3, 4, 16), historic trawl
#'   (2), IPHC (14), sablefish (35), and HBLL (22, 36, 39, 40).
#' @param major Character string (or vector) of major stat area code(s) to
#'   include (characters). Use get_major_areas() to lookup area codes with
#'   descriptions. Default is NULL.
#' @param usability A vector of usability codes to include. Defaults to NULL,
#'   but typical set for trawl is`c(0, 1, 2, 6)`. IPHC codes may be different to
#'   other surveys and the modern Sablefish survey doesn't seem to assign
#'   usabilities.
#' @param unsorted_only Defaults to FALSE, which will return all specimens
#'   collected on research trips. TRUE returns only unsorted (`1`) and `NA`
#'   specimens for both `species_category_code` and `sample_source_code`.
#' @param random_only Defaults to FALSE, which will return all specimens
#'   collected on research trips. TRUE returns only randomly sampled
#'   specimens (`sample_type_code` = `1, 2, 6, 7, or 8`).
#' @param grouping_only Defaults to FALSE, which will return all specimens or sets
#'   collected on research trips. TRUE returns only sets or specimens from fishing events
#'   with grouping codes that match that expected for a survey. Can also be
#'   achieved by filtering for specimens where `!is.na(grouping_code)`.
#' @param keep_all_ages Defaults to FALSE to keep only ages with standard methods
#'   for all surveys other than the NMFS Triennial.
#' @param include_event_info Logical for whether to append all relevant fishing
#'   event info (location, timing, effort, catch, etc.). Defaults to TRUE.
#' @param include_activity_matches TRUE gets all records collected with activity
#'   codes that match chosen ssids. Default is FALSE.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'   length or weight values and duplications due to trips that include multiple
#'   surveys. Default is TRUE.
#' @param remove_duplicates Remove duplicated specimen records due to overlapping
#'   survey stratifications when original_ind = 'N', or from known issues with
#'   MSSM trips including both survey areas.
#' @param return_dna_info Should DNA container ids and sample type be returned?
#'   This can create duplication of specimen ids for some species.  Defaults to
#'   FALSE.
#' @param return_specimen_type Should non-otolith structure types be returned?
#'   This can create duplication of specimen ids for some species.  Defaults to
#'   FALSE.
#' @param quiet_option Default option, `"message"`, suppresses messages from
#'   sections of code with lots of `join_by` messages. Any other string will allow
#'   messages.
#' @param drop_na_columns Logical for removing all columns that only contain NAs.
#'   Defaults to TRUE.
#'
#' @export
#'
#' @rdname get_all
get_all_survey_samples <- function(species, ssid = NULL,
                                   major = NULL,
                                   usability = NULL,
                                   unsorted_only = FALSE,
                                   random_only = FALSE,
                                   grouping_only = FALSE,
                                   keep_all_ages = FALSE,
                                   include_event_info = FALSE,
                                   include_activity_matches = FALSE,
                                   remove_bad_data = TRUE,
                                   remove_duplicates = TRUE,
                                   return_dna_info = FALSE,
                                   return_specimen_type = FALSE,
                                   drop_na_columns = TRUE,
                                   quiet_option = "message") {
  .q <- read_sql("get-all-survey-samples.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)

  if (!is.null(ssid)) {
    if (any(ssid %in% c(35, 41, 42, 43))) {
      ssid <- unique(c(ssid, 35, 41, 42, 43))
    }

    if (any(ssid %in% c(6, 7, 67))) {
      ssid_original <- ssid
      ssid <- unique(c(ssid, 6, 7, 67))
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

  ## ALTERNATE_SPECIMEN_TYPE can cause duplication for some species with multiple types collected per individual
  ## Could do something about record duplication with multiple samples like combining or not returning them?
  if (!return_specimen_type) {
    .d <- .d |>
      select(-ALTERNATE_SPECIMEN_TYPE) |>
      distinct()
  }

  names(.d) <- tolower(names(.d))

  if (nrow(.d) < 1) {
    if (is.null(ssid) & is.null(major)) {
      stop(paste0("No survey specimens for ", toString(species), "."))
    } else {
      if (!is.null(ssid) & is.null(major)) {
        stop(paste0("No survey specimens for ", toString(species), " from ssid(s) ", toString(ssid), "."))
      }
      if (is.null(ssid) & !is.null(major)) {
        stop(paste0("No survey specimens for ", toString(species), " from major area(s) ", toString(major), "."))
      }
      if (!is.null(ssid) & !is.null(major)) {
        stop(paste0("No survey specimens for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
      }
    }
  }

  ## dna_container_id and dna_sample_type can cause duplication for some species with multiple samples collected per individual
  ## Could do something about record duplication with multiple DNA samples like combining or not returning them?
  if (!return_dna_info) {
    .d <- .d |>
      select(-dna_container_id, -dna_sample_type) |>
      distinct()
  }

  ## populate length and length_type variables with most common type for each species ----

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

  ## ----

  # if using include_activity_matches = TRU`then remove_duplicates = TRUE
  if (include_activity_matches & !is.null(ssid)) {
    remove_duplicates <- TRUE
  }

  suppressMessages(
    if (remove_bad_data) {
      .d <- correct_ssids(.d, specimens = TRUE)
    },
    classes = quiet_option
  )


  if(!is.null(usability)|unsorted_only|random_only|grouping_only) {
    print(
      paste0("Looking for samples that are",
             ifelse(!is.null(usability), paste0( " usable (", toString(usability), ")"), ""),
             ifelse(unsorted_only, " unsorted", ""),
             ifelse(random_only, " random", ""),
             ifelse(grouping_only, " with originally specified grouping codes.", ".")
      )
    )
  }


  if (!is.null(ssid) & !include_activity_matches) {
    .d <- .d |>
      group_by(specimen_id, survey_series_id) |>
      mutate(
        grouping_desc_updated = ifelse(is.logical(na.omit(grouping_desc_updated)), NA, na.omit(grouping_desc_updated)),
        grouping_code_updated = mean(grouping_code_updated, na.rm = TRUE),
        grouping_code_updated = ifelse(is.nan(grouping_code_updated), NA, grouping_code_updated)
      ) |>
      dplyr::distinct() |>
      ungroup()


    if (any(ssid %in% c(6, 7, 67)) & !include_activity_matches) {
      ssid <- ssid_original
    }

    .d <- filter(.d, survey_series_id %in% ssid)

    if (is.null(major)) {
      print(
        paste0("Returning all ", toString(species), " specimens from survey series ", toString(ssid), ".")
      )
    }
    if (!is.null(major)) {
      print(
        paste0("Returning all ", toString(species), " specimens from within major area(s) ", toString(major), " and belonging to survey series ", toString(ssid), ".")
      )
    }
  } else {
    .d <- .d |>
      group_by(fishing_event_id) |>
      mutate(
        # make sure updated codes are from the original survey design and purge others
        grouping_desc_updated = ifelse(grouping_code_updated == grouping_code_original, grouping_desc_updated, NA),
        grouping_desc_updated = ifelse(is.logical(na.omit(grouping_desc_updated)), NA, na.omit(grouping_desc_updated)),
        grouping_code_updated = ifelse(grouping_code_updated == grouping_code_original, grouping_code_updated, NA),
        grouping_code_updated = mean(grouping_code_updated, na.rm = TRUE),
        grouping_code_updated = ifelse(is.nan(grouping_code_updated), NA, grouping_code_updated)
      ) |>
      dplyr::distinct() |>
      ungroup()


    if (is.null(major)) {
      print(
        paste0("Returning all ", toString(species), " specimens from all survey series.")
      )
    }
    if (!is.null(major)) {
      print(
        paste0("Returning all ", toString(species), " specimens from major area(s) ", toString(major), " from any survey series.")
      )
    }
  }

  if (unsorted_only) {
    # # .d <- filter(.d, sampling_desc == "UNSORTED")
    # # replaces SQL code
    # # (SPECIES_CATEGORY_CODE IS NULL OR SPECIES_CATEGORY_CODE IN (1, 3, 5, 6, 7)) AND
    # # (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE IN(1, 2)) AND
    # .d <- filter(.d, is.na(species_category_code) | species_category_code %in% c(1, 6))
    # # # only 1 = unsorted makes sense! 3 = keepers, 5 = remains, = 6 head only, 7 doesn't exist?
    # .d <- filter(.d, is.na(sample_source_code) | sample_source_code %in% c(1))
    # # # only 1 = unsorted makes sense! 2 = keepers, 3 = discards

    # TODO: try instead
    .d <- filter(
      .d, species_category_code %in% c(1) | # unsorted
        sample_source_code %in% c(1) | # unsorted
        sample_type_code %in% c(1) # total catch
    )

    if (nrow(.d) < 1) {
      if (is.null(ssid) & is.null(major)) {
        stop(paste0("No unsorted survey samples for ", toString(species), "."))
      } else {
        if (!is.null(ssid) & is.null(major)) {
          stop(paste0("No unsorted survey samples for ", toString(species), " from ssid(s) ", toString(ssid), "."))
        }
        if (is.null(ssid) & !is.null(major)) {
          stop(paste0("No unsorted survey samples for ", toString(species), " from major area(s) ", toString(major), "."))
        }
        if (!is.null(ssid) & !is.null(major)) {
          stop(paste0("No unsorted survey samples for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
        }
      }
    }
  } else {
    .ss <- get_table("Sample_Source") |> select(-ROW_VERSION)
    names(.ss) <- tolower(names(.ss))
    .d <- left_join(.d, .ss, by = "sample_source_code")
  }

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)

    if (nrow(.d) < 1) {
      if (is.null(ssid) & is.null(major)) {
        stop(paste0("No 'usable' survey samples for ", toString(species), "."))
      } else {
        if (!is.null(ssid) & is.null(major)) {
          stop(paste0("No 'usable' survey samples for ", toString(species), " from ssid(s) ", toString(ssid), "."))
        }
        if (is.null(ssid) & !is.null(major)) {
          stop(paste0("No 'usable' survey samples for ", toString(species), " from major area(s) ", toString(major), "."))
        }
        if (!is.null(ssid) & !is.null(major)) {
          stop(paste0("No 'usable' survey samples for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
        }
      }
    }
  }

  .u <- get_table("Usability") |> select(-ROW_VERSION)
  names(.u) <- tolower(names(.u))
  .d <- left_join(.d, .u, by = "usability_code")

  if (random_only) {
    # replaces SQL code
    # SM.SAMPLE_TYPE_CODE IN (1, 2, 6, 7, 8) AND
    .d <- filter(.d, sample_type_code %in% c(1, 2, 6, 7, 8)) # 8 = random from set requested by vessel master

    if (nrow(.d) < 1) {
      if (is.null(ssid) & is.null(major)) {
        stop(paste0("No random survey samples for ", toString(species), "."))
      } else {
        if (!is.null(ssid) & is.null(major)) {
          stop(paste0("No random survey samples for ", toString(species), " from ssid(s) ", toString(ssid), "."))
        }
        if (is.null(ssid) & !is.null(major)) {
          stop(paste0("No random survey samples for ", toString(species), " from major area(s) ", toString(major), "."))
        }
        if (!is.null(ssid) & !is.null(major)) {
          stop(paste0("No random survey samples for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
        }
      }
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

  if(!keep_all_ages){
  .d <- .d %>%
    mutate(
      age = case_when(
        species_ageing_group == "rockfish_flatfish_hake" & ageing_method_code %in% c(1, 3, 16, 17) ~ .d$age,
        species_ageing_group == "sharks_skates" & ageing_method_code %in% c(12) ~ .d$age,
        species_ageing_group == "dogfish" & ageing_method_code %in% c(11) ~ .d$age,
        species_ageing_group == "pcod_lingcod" & ageing_method_code %in% c(6) ~ .d$age,
        species_ageing_group == "pollock" & ageing_method_code %in% c(7) ~ .d$age,
        species_ageing_group == "shortraker_thornyheads" & ageing_method_code %in% c(1, 3, 4, 16, 17) ~ .d$age,
        survey_series_id == 79 ~ .d$age,
        !is.na(species_ageing_group) & is.na(ageing_method_code) & survey_series_id != 79 ~ NA_real_
      )
    )
  }

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
    print("Specimens found. Fetching additional event info.")
  }

  suppressMessages(
    if (include_event_info) {
      options(scipen = 999)
      # needed for big skate because of a MSA set with an id that was getting converted

      .f <- .d %>% filter(!is.na(fishing_event_id))
      fe_vector <- unique(na.omit(.f$fishing_event_id))

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

      ssid_with_samples <- unique(na.omit(.d$survey_series_id))

      .fe <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid_with_samples,
        sql_code = .fe,
        search_flag = "-- insert ssid here", conversion_func = I
      )

      ## this didn't work if we want all the sub level and minor level ids
      # .fe <- inject_filter("AND FE.FE_PARENT_EVENT_ID IS NULL OR IN", fe_vector,
      #                      sql_code = .fe,
      #                      search_flag = "-- insert fe_vector here", conversion_func = I
      # )

      # so reduce data size using trip ids instead?
      trip_vector <- unique(na.omit(.d$trip_id))

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

      if (any(!is.na(fe$FE_SUB_LEVEL_ID))) {
        if (any(na.omit(fe$FE_SUB_LEVEL_ID) > 1)) {
          # get both parent and skate level counts
          fe1 <- get_parent_level_counts(fe)
          fe2 <- get_skate_level_counts(fe)
        } else {
          fe1 <- get_parent_level_counts(fe)
        }
      } else {
        fe1 <- get_parent_level_counts(fe)
      }

      fe1 <- fe1 |>
        select(
          -SURVEY_SERIES_ID,
          -SURVEY_SERIES_OG,
          -SURVEY_ID,
          -MINOR_STAT_AREA_CODE, # some sub level events had NAs here
          -REASON_DESC, -USABILITY_CODE,
          -GROUPING_CODE_ORIGINAL, -GROUPING_DESC_ORIGINAL,
          -GROUPING_CODE_UPDATED, -GROUPING_DESC_UPDATED, -ORIGINAL_IND
        ) |>
        distinct() # avoid clashing with values for samples

      names(fe1) <- tolower(names(fe1))

      if (any(na.omit(fe$FE_SUB_LEVEL_ID) > 1)) {
        fe2 <- fe2 |>
          select(
            -SURVEY_SERIES_ID,
            -SURVEY_SERIES_OG,
            -SURVEY_ID,
            -MINOR_STAT_AREA_CODE,
            -REASON_DESC, -USABILITY_CODE,
            -GROUPING_CODE_ORIGINAL, -GROUPING_DESC_ORIGINAL,
            -GROUPING_CODE_UPDATED, -GROUPING_DESC_UPDATED, -ORIGINAL_IND
          ) |>
          distinct() # avoid clashing with values for samples

        count_gear_types <- fe2 |> group_by(fishing_event_id) |>
          summarise(max = max(
            ### can add any more gear variables needed here
            sum(!is.na(unique(HOOK_CODE))),
            sum(!is.na(unique(HOOKSIZE_DESC)))
          ))

        names(fe2) <- tolower(names(fe2))

        # do any of those sub levels differ in gear type?
        ## TODO: other possible variables and tests for difference could be added
        if (max(count_gear_types$max, na.rm = TRUE) < 2) {
          # NO, then use only the parent level values
          .d <- left_join(.d, fe1)
        } else {
          # YES, then get skate level catch for each species
          slc_list <- list()
          spp_codes <- unique(.d$species_code)
          for (i in seq_along(spp_codes)) {
            .slc <- read_sql("get-sub-level-catch.sql")
            .slc <- inject_filter("", spp_codes[i], sql_code = .slc)
            .slc <- inject_filter("AND C.SPECIES_CODE IN", spp_codes[i],
              sql_code = .slc,
              search_flag = "-- insert species again here"
            )
            slc_list[[i]] <- run_sql("GFBioSQL", .slc)
          }
          slc <- do.call(rbind, slc_list) |> distinct()
          names(slc) <- tolower(names(slc))

          .d2 <- .d |>
            left_join(count_gear_types) |>
            # need to include both tests because rarely fe_sub_level_ids can be missing
            filter(max > 1 & !is.na(fe_sub_level_id)) |>
            left_join(fe2) |>
            # select(-survey_series_id) |>
            select(-max) |>
            distinct()

          # but only replace event level when multiple gear types are present
          .d2 <- .d2 %>%
            # select(-catch_count) |>
            rename(event_level_count = catch_count) |> # used as a temporary check
            left_join(slc, by = c(
              "trip_id" = "trip_id",
              "fishing_event_id" = "fe_parent_event_id",
              "fe_major_level_id" = "fe_major_level_id",
              "fe_sub_level_id" = "fe_sub_level_id",
              "species_code" = "species_code"
            ))

          check_counts <- select(.d2, fishing_event_id, skate_id, event_level_count, catch_count) |>
            distinct() |>
            group_by(fishing_event_id) |>
            mutate(missing_skates = event_level_count - catch_count) |>
            ungroup()

          # check if any skate-level catch_counts exceed event_level_counts or are missing
          if(min(check_counts$missing_skates)<0|any(is.na(.d2$catch_count))) {
            warning("Some skate-level counts are inconsistent with counts for events with gear differences.")
          } else {
            # .d2 <- .d2 |> select(-event_level_count)
          }

          .d1 <- .d |>
            left_join(count_gear_types) |>
            # need to include both tests because rarely fe_sub_level_ids can be missing
            filter(max < 2 | is.na(fe_sub_level_id)) |>
            left_join(fe1) |>
            # select(-survey_series_id) |>
            select(-max) |>
            distinct()

          # check for missing data
          if(nrow(.d1) + nrow(.d2) == nrow(.d)) {
            .d <- bind_rows(.d1, .d2)
          } else {
            warning("Event data appears to be missing for some specimens. Check output carefully.")
            .d3 <- bind_rows(.d1, .d2)
            .d <- left_join(.d, .d3)
          }

        }
      } else {
        .d <- left_join(.d, fe1)
      }

      .d <- .d |>
        group_by(specimen_id) |>
        mutate(
          doorspread_m = ifelse(is.logical(na.omit(doorspread_m)), NA, na.omit(doorspread_m)),
          speed_mpm = ifelse(is.logical(na.omit(speed_mpm)), NA, na.omit(speed_mpm))
        ) |>
        dplyr::distinct() |>
        ungroup()


      # in trawl data, catch_count is only recorded for small catches
      # so 0 in the catch_count column when catch_weight > 0 seems misleading
      # note: there are also a few occasions for trawl where count > 0 and catch_weight is 0/NA
      # these lines replace false 0s with NA, but additional checks might be needed

      .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
      .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)

      .d <- trawl_area_swept(.d)

      .d <- hook_area_swept(.d)

    },
    classes = quiet_option
  )


  .d <- .d |>
    relocate(species_common_name, survey_series_id, sex, length, weight, age) |>
    arrange(species_common_name, survey_series_id, -fishing_event_id)

  if (grouping_only) {
    .d <- filter(.d, !is.na(grouping_code_original))
  }

  if (nrow(.d) < 1) {
    if (is.null(ssid) & is.null(major)) {
      stop(paste0("No survey samples with expected grouping codes for ", toString(species), "."))
    } else {
      if (!is.null(ssid) & is.null(major)) {
        stop(paste0("No survey samples with expected grouping codes for ", toString(species), " from ssid(s) ", toString(ssid), "."))
      }
      if (is.null(ssid) & !is.null(major)) {
        stop(paste0("No survey samples with expected grouping codes for ", toString(species), " from major area(s) ", toString(major), "."))
      }
      if (!is.null(ssid) & !is.null(major)) {
        stop(paste0("No survey samples with expected grouping codes for ", toString(species), " from ssid(s) ", toString(ssid), " in major area(s) ", toString(major), "."))
      }
    }
  }

  if (drop_na_columns) {
    .d <- .d %>% select(where(~ !all(is.na(.x))))
  }

  .d <- unique(.d)

  # check if there are duplicate specimen ids
  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
    if (remove_duplicates) {
      # if so, separate original_ind from not
      .dy <- filter(.d, original_ind == "Y")
      .dn <- filter(.d, original_ind != "Y" | is.na(original_ind))

      # and only keep those not original_ind = Y when the specimen id was missing
      .d <- bind_rows(.dy, filter(.dn, !(specimen_id %in% c(unique(.dy$specimen_id)))))
      .d <- filter(.d, !(survey_series_id == 0 & (specimen_id %in% c(unique(.dy[duplicated(.dy$specimen_id),]$specimen_id)))))
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

  # TODO: add a check to see if ssid and ssog are identical and drop ssog if so?

  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  # we will use grouping_code_original as the primary grouping_code returned
  .d <- dplyr::rename(.d, grouping_code = grouping_code_original, grouping_desc = grouping_desc_original)

  add_version(as_tibble(.d))
}
