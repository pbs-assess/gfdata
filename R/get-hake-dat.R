#' Extract hake survey samples from GFBIOSQL
#'
#' @export
#' @rdname get_data
get_hake_survey_samples <- function() {
  .q <- read_sql("get-hake-survey-samples.sql")
  length_type <- get_spp_sample_length_type(225)
  search_flag <- "-- insert length type here"
  i <- grep(search_flag, .q)
  .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  as_tibble(.d)
}

#' Get the hake catch from the Oracle FOS database
#' @param end_date A string representing the date. Must be of the format dd/mm/yyyy
#'
#' @export
#' @rdname get_data
get_hake_catch <- function(end_date = format(Sys.Date(), "%d/%m/%Y")){
  .q <- read_sql("get-hake-catch.sql")
  .q <- gsub("--to-dd-mm-yyyy--", end_date, .q)

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  .d$year <- lubridate::year(.d$best_date)

  ft <- toupper(c(
    "Viking Enterprise",
    "Northern Alliance",
    "Osprey #1",
    "Raw Spirit",
    "Viking Alliance"
  ))

  .d <- .d %>% mutate(
    hake_fishery =
      case_when(
        .data$trip_type_code == 12766 ~ "JV",
        .data$trip_type_code == 12764 & .data$vessel_name %in% ft ~ "FT",
        .data$trip_type_code == 12764 & !.data$vessel_name %in% ft ~ "SS"
      )
  )

  .d <- .d %>%
    filter(.data$landed_kg > 0) %>%
    filter(.data$major_stat_area_code %in% c("03", "04", "05", "06", "07", "08", "09") |
             (.data$major_stat_area_code == "01" & .data$minor_stat_area_code == "20")) %>%
    filter(.data$gear == "MIDWATER TRAWL")

  # if (modern) {
  #   .d <- .d %>%
  #     filter(.data$trip_type_code %in% c(
  #       12764, # OPT A - HAKE QUOTA (SHORESIDE)
  #       12766
  #     )) # OPT B - HAKE QUOTA (JV)
  # }

  as_tibble(.d)
}
