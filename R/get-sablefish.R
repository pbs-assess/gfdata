#' Extract sablefish survey data from GFBIOSQL
#'
#' @export
#' @rdname get_data
get_sablefish_surveys <- function() {
  .q <- read_sql("get-sablefish-survey-data.sql")
  # length_type <- get_spp_sample_length_type(225)
  # search_flag <- "-- insert length type here"
  # i <- grep(search_flag, .q)
  # .q[i] <- paste0("CAST(ROUND(", length_type, "/ 10, 1) AS DECIMAL(8,1)) AS LENGTH,")

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  add_version(as_tibble(.d))
}
