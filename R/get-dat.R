#' Get PBS data
#'
#' Automates fisheries and research survey data extraction from DFO Pacific
#' groundfish databases. The output datasets feed into other functions (`tidy_`,
#' `plot_`, or `fit_` functions) for data visualization, which can be used as
#' products themselves or can be fed into automated DFO Pacific groundfish data
#' synopsis report production.
#'
#' @details
#' * `get_survey_sets()` extracts survey catch data and spatial data for
#'    plotting survey catchs on a map of British Columbia
#'
#' * `get_survey_samples()` extracts all biological sample specimen records
#'    from research surveys for given species and survey series IDs from GFBio
#'
#' * `get_hake_survey_samples()` extracts all biological sample specimen records
#'    from hake joint acoustic surveys from GFBio
#'
#' * `get_commercial_samples()` extracts all biological sample specimen records
#'    from commercial data for given species from GFBio
#'
#' * `get_catch()` extracts all landing and discard records for a given species
#'    from GFFOS.GF_MERGED_CATCH
#'
#' * `get_hake_catch()` extracts all landing and discard records for Pacific Hake
#'    with some extra data used in the Hake assessment
#'
#' * `get_cpue_spatial()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the groundfish trawl fishery
#'
#' * `get_cpue_spatial_ll()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the longline fishery
#'
#' * `get_cpue_index()` extracts catch and effort data from
#'    GFFOS.GF_MERGED_CATCH for the groundfish trawl fishery since 1996
#'
#' * `get_cpue_historical()` extracts historical catch and effort data back into
#'    the 1950s. It's help file is on a separate page; see the link
#'
#' * `get_age_precision()` extracts age readings from biological samples for a
#'    given species where there is a second ('precision') age reading
#'
#' * `get_sara_dat()` scrubs Species At Risk website for up-to-date species
#'    status and listings
#'
#' * `get_survey_index()` extracts survey catch data for given species
#'    and survey series IDs
#'
#' * `get_management()` extracts management actions
#'
#' * `cache_pbs_data()` runs all 'get' functions in the gfdata package
#'
#'    (except those specific to IPHC data) and caches extracted data to a given
#'    folder
#'
#' @section Authentication:
#' `get_*` functions only extract data when performed on a computer connected to
#' the Pacific Biological Station DFO network. By default, the functions assume
#' that you are on an authorized DFO Windows computer where authentication with
#' the databases happens automatically. If instead, you wish to connect by
#' username and password, see the details section in [run_sql()].
#'
#' @examples
#' \dontrun{
#' ## Import survey catch density and location data by tow or set for plotting
#' ## Specify single or multiple species by common name or species code and
#' ## single or multiple survey series id(s).
#' get_survey_sets(species = "lingcod", ssid = 1)
#'
#' ## Import survey or commercial biological data for various plots
#' ## (e.g. length frequency, growth, age frequency, maturity, etc.)
#' get_survey_samples(species = 442, ssid = c(1, 3, 4, 16))
#'
#' get_commercial_samples(c(442, 397))
#'
#' ## Import catch data by species for barcharts of landings by fishing area,
#' ## geartype, and year.
#' get_catch("lingcod")
#'
#' ## Import spatial commercial catch per unit effort data for trawl or longline
#' ## data by species for plotting along BC coast.
#' get_cpue_spatial("lingcod")
#' get_cpue_spatial_ll("yelloweye rockfish")
#'
#' ## Import catch and effort data by gear type for modelling commercial trawl
#' ## cpue index.
#' get_cpue_index(gear = "bottom trawl", min_cpue_year = 2012)
#'
#' ## Import survey bootstrapped biomass estimates for plotting relative biomass
#' ## indices by specified survey series.
#' get_survey_index("pacific cod", ssid = c(1, 3, 4, 16))
#' }
#'
#' @param species One or more species common names (e.g. `"pacific ocean
#'   perch"`) or one or more species codes (e.g. `396`). Species codes can be
#'   specified as numeric vectors `c(396, 442`) or characters `c("396", "442")`.
#'   Numeric values shorter than 3 digits will be expanded to 3 digits and
#'   converted to character objects (`1` turns into `"001"`). Species common
#'   names and species codes should not be mixed. If any element is missing a
#'   species code, then all elements will be assumed to be species common
#'   names.
#' @param ssid A numeric vector of survey series IDs. Run [get_ssids()] for a
#'   look-up table of available survey series IDs with surveys series
#'   descriptions.
#' @name get_data
NULL


#' @export
#' @param join_sample_ids If `TRUE` then the sample IDs will be joined in. This
#'   may result in repeated rows of data if the same sample ID is part of
#'   different survey stratifications.
#' @param verbose If `TRUE` then extra messages were reprinted during data
#'   extraction. Useful to monitor progress.
#' @param sleep System sleep in seconds between each survey-year
#'   to be kind to the server.
#' @rdname get_data
get_survey_sets <- function(species, ssid = c(1, 3, 4, 16, 2, 14, 22, 36),
                            join_sample_ids = FALSE, verbose = FALSE,
                            sleep = 0.05) {
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

  missing <- setdiff(ssid, c(trawl, ll))
  if (length(missing) > 0) {
    stop("ssid(s) ", missing, " is/are not supported. Must be one of ",
      paste(sort(c(trawl, ll)), collapse = ", "), ". ",
      "See the function `get_ssids()` for help identifying ",
      "survey series IDs.",
      call. = FALSE
    )
  }

  species_codes <- common2codes(species)

  species_df <- run_sql("GFBioSQL", "SELECT * FROM SPECIES")
  sample_trip_ids <- get_sample_trips()
  areas <- get_strata_areas()
  survey_ids <- get_survey_ids(ssid)
  surveys <- get_ssids()

  ## STILL NEED TO ADD
  # CAPTAIN_ID,
  # VESSEL_ID,

  fe <- run_sql("GFBioSQL", "SELECT
    FISHING_EVENT_ID,
    MONTH(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS MONTH,
    DAY(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS DAY,
    FE_END_DEPLOYMENT_TIME AS TIME_DEPLOYED,
    FE_BEGIN_RETRIEVAL_TIME AS TIME_RETRIEVED,
    FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS
      LONGITUDE,
    FE_END_LATTITUDE_DEGREE + FE_END_LATTITUDE_MINUTE / 60 AS LATITUDE_END,
    -(FE_END_LONGITUDE_DEGREE + FE_END_LONGITUDE_MINUTE / 60) AS
      LONGITUDE_END,
      FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
    FROM FISHING_EVENT FE
    INNER JOIN TRIP T ON T.TRIP_ID = FE.TRIP_ID")

  Sys.sleep(sleep)

  d_survs <- list()
  k <- 0
  for (i in seq_along(species_codes)) {
    for (j in seq_along(survey_ids$SURVEY_ID)) {
      if (survey_ids$SURVEY_SERIES_ID[j] %in% trawl) {
        sql_proc <- "proc_catmat_2011"
      }
      if (survey_ids$SURVEY_SERIES_ID[j] %in% ll) {
        sql_proc <- "proc_catmat_ll_2013"
      }

      k <- k + 1
      if (verbose) {
        message(
          "extracting data for survey ID ", survey_ids$SURVEY_ID[j],
          " and species code ", species_codes[i]
        )
      }
      d_survs[[k]] <- run_sql(
        "GFBioSQL",
        paste0(
          "EXEC ", sql_proc, " ", survey_ids$SURVEY_ID[j], ", '",
          species_codes[i], "'"
        )
      )
      Sys.sleep(sleep)
    }
  }
  .d <- bind_rows(d_survs)

  if (nrow(.d) < 1) {
    stop("No survey set data for selected species.")
  }

  .d <- inner_join(.d,
    unique(select(
      surveys,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC,
      SURVEY_ABBREV
    )),
    by = "SURVEY_SERIES_ID"
  )

  .d <- inner_join(.d,
    unique(select(
      fe,
      FISHING_EVENT_ID,
      MONTH,
      DAY,
      TIME_DEPLOYED,
      TIME_RETRIEVED,
      LATITUDE_END,
      LONGITUDE_END
    )),
    by = "FISHING_EVENT_ID"
  )

  .d <- inner_join(.d,
    unique(select(
      species_df,
      SPECIES_CODE,
      SPECIES_COMMON_NAME,
      SPECIES_SCIENCE_NAME,
      SPECIES_DESC
    )),
    by = "SPECIES_CODE"
  )

  if (join_sample_ids) {
    # give us each sample_id associated with each fishing_event_id and species:
    .d <- left_join(.d, sample_trip_ids,
      by = c("SPECIES_CODE", "FISHING_EVENT_ID")
    ) %>%
      left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))
  }

  names(.d) <- tolower(names(.d))
  .d <- mutate(.d,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name)
  )

  missing_species <- setdiff(species_codes, .d$species_code)
  if (length(missing_species) > 0) {
    warning(
      "The following species codes do not have survey set data in GFBio.",
      paste(missing_species, collapse = ", ")
    )
  }
  as_tibble(.d)
}

#' @export
#'
#' @rdname get_data
get_ll_hook_data <- function(species = NULL, ssid = NULL){
  .q <- read_sql("get-ll-hook-data.sql")
  .q <- inject_filter("", species, sql_code = .q)
  if (!is.null(ssid)) {
    .q <- inject_filter(" ", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  # .d$species_common_name <- tolower(.d$species_common_name)
  # .d$species_science_name <- tolower(.d$species_science_name)

  as_tibble(.d)
}

#' @export
#' @rdname get_data
#' @param major Character string (or vector, though doesn't work yet with
#'  `cache_pbs_data`) of major stat area code to include (characters). Use
#'  get_major_areas() to lookup area codes with descriptions.
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
get_survey_samples <- function(species, ssid = NULL, remove_bad_data = TRUE,
                               unsorted_only = TRUE, usability = NULL, major = NULL) {
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

  as_tibble(.d)
}

#' @export
#' @param unsorted_only Remove sorted biological data ('keepers' and 'discards'
#'  and unknown). Default = TRUE.
#' @rdname get_data
get_commercial_samples <- function(species, unsorted_only = TRUE, major = NULL,
                                   usability = NULL) {
  .q <- read_sql("get-comm-samples.sql")
  .q <- inject_filter("AND SM.SPECIES_CODE IN", species, sql_code = .q)

  length_type <- get_spp_sample_length_type(species)
  message(paste0("All or majority of length measurements are ", length_type))
  search_flag <- "-- insert length type here"
  i <- grep(search_flag, .q)
  .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")
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

  as_tibble(.d)
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
  as_tibble(.d)
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

#' Get all fishing catch and effort to calculate historical commercial CPUE
#'
#' @param alt_year_start_date Alternative year starting date specified as a
#'   month-day combination. E.g. "03-01" for March 1st. Can be used to create
#'   'fishing years'.
#' @param areas Area groupings as a vector of regular expressions.
#'   See [base::regex()].
#' @param end_year Specify the last calendar year to be extracted.
#' @export
#' @rdname get_data
get_cpue_historical <- function(species = NULL, major = NULL,
                                alt_year_start_date = "04-01", areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
                                end_year = NULL) {
  .q <- read_sql("get-cpue-historic.sql")
  if (!is.null(species)) {
    .q <- inject_filter("AND MC.SPECIES_CODE IN", species, sql_code = .q)
  }
  if (!is.null(major)) {
    .q <- inject_filter("AND MC.MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql(database = c("GFFOS", "GFCatch", "PacHarvest"), .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d <- rename(.d, total = totcatch_kg, minor_stat_area_code = min)
  .d$hours_fished <- as.numeric(as.character(.d$hours_fished))
  .d$database_name <- tolower(.d$database_name)
  .d$gear <- tolower(.d$gear)
  .d$locality_description <- tolower(.d$locality_description)


  # Filter out fishing records after last year required
  if (!is.null(end_year)) {
    .d <- .d %>% filter(year <= end_year)
  }

  # Create possibly alternate starting date:

  if (alt_year_start_date != "01-01") {
    .d <- dplyr::mutate(.d,
      .year_start_date =
        lubridate::ymd_hms(paste0(year, "-", alt_year_start_date, " 00:00:00"))
    )
    .d <- dplyr::mutate(.d, .time_diff = best_date - .year_start_date)
    .d <- dplyr::mutate(.d, alt_year = ifelse(.time_diff > 0, year, year - 1L))
    .d <- dplyr::select(.d, -.time_diff, -.year_start_date)
  }

  .d$area <- gfdata::assign_areas(.d$major_stat_area_description, areas)
  .d$specific_area <- gfdata::assign_areas(.d$major_stat_area_description,
    area_regex = c("3C", "3D", "5A", "5B", "5C", "5D", "5E")
  )

  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_cpue_historical_hake <- function(end_year = NULL) {
  .q <- read_sql("get-cpue-historical-hake.sql")
  .d <- run_sql(database = c("GFFOS", "GFCatch", "PacHarvest"), .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d <- rename(.d, total = totcatch_kg, minor_stat_area_code = min)
  .d$hours_fished <- as.numeric(as.character(.d$hours_fished))
  .d$database_name <- tolower(.d$database_name)
  .d$gear <- tolower(.d$gear)
  .d$locality_description <- tolower(.d$locality_description)


  # Filter out fishing records after last year required
  if (!is.null(end_year)) {
    .d <- .d %>% filter(year <= end_year)
  }

  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_cpue_historical_hl <- function(species = NULL, major = NULL,
                                   alt_year_start_date = "04-01", areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
                                   end_year = NULL) {
  .q <- read_sql("get-cpue-historic-hl-beta.sql")
  if (!is.null(species)) {
    .q <- inject_filter("AND MC.SPECIES_CODE IN", species, sql_code = .q)
  }
  if (!is.null(major)) {
    .q <- inject_filter("AND MC.MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql(database = "GFFOS", .q)
  # .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
  #  toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_cpue_spatial <- function(species, major = NULL) {
  .q <- read_sql("get-cpue-spatial.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  if (!is.null(major)) {
    .q <- inject_filter("AND MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_catch_spatial <- function(species, major = NULL) {
  .q <- read_sql("get-catch-spatial.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  if (!is.null(major)) {
    .q <- inject_filter("AND MAJOR_STAT_AREA_CODE =", major, .q,
                        search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_cpue_spatial_ll <- function(species, major = NULL) {
  .q <- read_sql("get-cpue-spatial-ll.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  if (!is.null(major)) {
    .q <- inject_filter("AND C.MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$gear <- tolower(.d$gear)
  .d$fishery_sector <- tolower(.d$fishery_sector)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  as_tibble(.d)
}

#' @param gear The gear type(s) to include for CPUE. Will be converted to
#'  uppercase. Run [get_comm_gear_types()] for a look-up table of available
#'  gear types to select from.
#' @param min_cpue_year Minimum year for the CPUE data.
#' @export
#' @rdname get_data
get_cpue_index <- function(gear = "bottom trawl", min_cpue_year = 1996,
  major = NULL) {
  .q <- read_sql("get-cpue-index.sql")
  i <- grep("-- insert filters here", .q)
  .q[i] <- paste0(
    "WHERE GEAR IN(", collapse_filters(toupper(gear)),
    ") AND YEAR(BEST_DATE) >= ", min_cpue_year
  )
  if (!is.null(major)) {
    .q <- inject_filter("AND MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_cpue_index_hl <- function(min_cpue_year = 1980, major = NULL) {
  .q <- read_sql("get-cpue-index-hl.sql")
  i <- grep("-- insert filters here", .q)
  .q[i] <- paste0(
    "AND YEAR(BEST_DATE) >= ", min_cpue_year
  )
  if (!is.null(major)) {
    .q <- inject_filter("AND MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

get_commercial_hooks_per_fe <- function(min_cpue_year = 2008) {
  .q <- read_sql("get-commercial-hooks-per-fe.sql")
  i <- grep("-- insert filters here", .q)
  .q[i] <- paste0(
    "AND YEAR(BEST_DATE) >= ", min_cpue_year
  )
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get_data
#' @param major To select only the inside population (Strait of Georgia, area
#'  4B only), set inside = 1. To select only the outside population, set inside
#'   = 0.
get_age_precision <- function(species, major = NULL) {
  .q <- read_sql("get-age-precision.sql")
  .q <- inject_filter("AND SM.SPECIES_CODE IN", species, .q)
  if (!is.null(major)) {
    .q <- inject_filter("AND MAJOR_STAT_AREA_CODE =", major, .q,
      search_flag = "-- insert major here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d <- .d %>% select(-.data$major_stat_area_code)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_survey_index <- function(species, ssid = NULL) {
  .q <- read_sql("get-survey-index.sql")
  .q <- inject_filter("WHERE SP.SPECIES_CODE IN", species, .q)
  if (!is.null(ssid)) {
    .q <- inject_filter("AND BD.SURVEY_SERIES_ID IN", ssid, .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)
  as_tibble(.d)
}


#' @export
#' @rdname get_data
get_sable_landings <- function(species, ssid = NULL) {
  .q <- read_sql("get-sablefish-landings.sql")
  .d <- run_sql("Sablefish", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}


#' @export
#' @rdname get_data
get_survey_blocks <- function(ssid = NULL) {
  .q <- read_sql("get-survey-blocks.sql")

  if (is.null(ssid)) {
    stop("Please specify a survey series id.")
  }
  .q <- inject_filter("AND SURVEY_SERIES_ID IN", ssid, .q,
    search_flag = "-- insert ssid here", conversion_func = I
  )

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d <- dplyr::distinct(.d)
  .d$block_designation <- as.numeric(.d$block_designation)
  as_tibble(.d)
}


#' @export
#' @rdname get_data
get_eulachon_specimens <- function() {
  .q <- read_sql("get-eulachon-specimens.sql")
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_gear_types <- function() {
  .d <- run_sql(
    "GFFOS",
    "SELECT GEAR
    FROM GFFOS.dbo.GF_MERGED_CATCH C
    GROUP BY GEAR"
  )
  names(.d) <- tolower(names(.d))
  .d$gear <- tolower(.d$gear)
  as_tibble(.d)
}

#' @param species_group Species group code(s) to include (see lookup table
#'   [get_species_groups()]). Defaults to all.
#' @param fishery The fishery_id code(s) (see lookup table [get_fishery_ids()])
#'   for fisheries to include in data extraction. Defaults to all.
#' @param area The fishery area(s) (see lookup table [get_management_areas()])
#'   to include in data extraction (eg. '5A'; c('3C', '3D', '5A', '5B')).
#' @param start_year The minimum year to include management actions.
#'   Defaults to all.
#' @export
#' @rdname get_data
get_management <- function(species = NULL, species_group = NULL, fishery = NULL,
                           area = NULL, start_year = NULL) {
  .q <- read_sql("get-management.sql")
  if (!is.null(species)) {
    .q <- inject_filter("AND Species_Code IN", species, .q,
      search_flag = "-- insert species here", conversion_func = common2codes
    )
  }
  if (!is.null(fishery)) {
    .q <- inject_filter("AND M.Fishery_Id IN", fishery, .q,
      search_flag = "-- insert fishery here", conversion_func = I
    )
  }
  if (!is.null(species_group)) {
    .q <- inject_filter("AND M.Species_Group_Code IN", species_group, .q,
      search_flag = "-- insert species group here", conversion_func = I
    )
  }
  if (!is.null(area)) {
    .q <- inject_filter("AND MA.Area_Code IN", area, .q,
      search_flag = "-- insert area here", conversion_func = I
    )
  }
  if (!is.null(start_year)) {
    .q <- inject_filter("AND YEAR(Action_Start_Date) >=", start_year, .q,
      search_flag = "-- insert start year here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  as_tibble(.d)
  .d %>% arrange(dplyr::desc(action_start_date))
}

#' @export
#' @rdname get_data
get_sara_dat <- function() {
  .h <- xml2::read_html(
    "http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm"
  )
  .d <- .h %>%
    rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    .[-(1:2), ] %>%
    dplyr::as_tibble() %>%
    filter(.data$Taxon %in% "Fishes") %>%
    filter(!grepl("Salmon", .data$`Common name *`))
  names(.d) <- tolower(names(.d))
  names(.d) <- gsub(" ", "_", names(.d))
  names(.d) <- gsub("_\\*", "", names(.d))
  as_tibble(.d)
}

#' @param file_name Optional filename(s) for the cached file. Defaults to the
#'   same as the `species` argument.
#' @param path The folder where the cached data will be saved.
#' @param compress Compress the `.rds` file? Defaults to `FALSE` for faster
#'   reading and writing at the expense of disk space.
#' @param historical_cpue Logical for whether historical CPUE should be included.
#' @param survey_sets Logical for whether the survey set data should be
#'   extracted. You might set this to `FALSE` if you don't need these data and
#'   you want to substantially speed up data extraction.
#' @export
#' @return The `get_*` functions return a data frame. The [cache_pbs_data()]
#' function writes an `.rds` file to `path` for each specified species. A data
#' object for a single species is a named list object with each element
#' containing a data frame from a `get_*` function. The element name of the list
#' reflects the function name with the `get_` part removed. For example, the
#' output from [get_survey_samples()] is in a list element named
#' `survey_samples()`.
#' @details
#' This [cache_pbs_data()] function caches data from
#' * [get_survey_samples()]
#' * [get_commercial_samples()]
#' * [get_catch()]
#' * [get_cpue_spatial()]
#' * [get_cpue_spatial_ll()]
#' * [get_catch_spatial()]
#' * [get_survey_index()]
#' * [get_age_precision()]
#' * and optionally from [get_survey_sets()] and [get_cpue_historical()]

#' @rdname get_data
cache_pbs_data <- function(species, major = NULL, file_name = NULL, path = ".",
                           compress = FALSE, unsorted_only = TRUE, historical_cpue = FALSE,
                           survey_sets = FALSE, verbose = TRUE) {
  dir.create(path, showWarnings = FALSE)
  for (sp_i in seq_along(species)) {
    this_sp <- species[[sp_i]]

    if (is.null(file_name)) {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", this_sp))
    } else {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", file_name[[sp_i]]))
    }
    message("Extracting data for ", codes2common(this_sp))
    out <- list()
    if (survey_sets) {
      out$survey_sets <- get_survey_sets(this_sp,
        join_sample_ids = TRUE,
        verbose = verbose
      )
    }
    if (historical_cpue) {
      out$cpue_historical <- get_cpue_historical(this_sp)
    }
    out$survey_samples <- get_survey_samples(this_sp)
    out$commercial_samples <- get_commercial_samples(this_sp,
      unsorted_only = unsorted_only
    )
    out$catch <- get_catch(this_sp)
    out$cpue_spatial <- get_cpue_spatial(this_sp)
    out$cpue_spatial_ll <- get_cpue_spatial_ll(this_sp)
    out$catch_spatial <- get_catch_spatial(this_sp)
    out$survey_index <- get_survey_index(this_sp)
    # out$management         <- get_management(this_sp)
    out$age_precision <- get_age_precision(this_sp)
    if(is.null(major)) {
    saveRDS(out,
      file = paste0(file.path(path, this_sp_clean), ".rds"),
      compress = compress
    )
    } else {
      saveRDS(out,
        file = paste0(file.path(path, paste0(this_sp_clean, "_", major)), ".rds"),
        compress = compress
      )
    }
  }
  message("All data extracted and saved in the folder `", path, "`.")
}
