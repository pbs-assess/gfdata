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

  fe <- run_sql("GFBioSQL", "SELECT
    FISHING_EVENT_ID,
    T.VESSEL_ID AS VESSEL_ID,
    T.CAPTAIN_ID AS CAPTAIN_ID,
    MONTH(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS MONTH,
    DAY(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS DAY,
    ISNULL(FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME) AS TIME_DEPLOYED,
    ISNULL(FE_END_BOTTOM_CONTACT_TIME, FE_BEGIN_RETRIEVAL_TIME) AS TIME_RETRIEVED,
    FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS
      LONGITUDE,
    FE_END_LATTITUDE_DEGREE + FE_END_LATTITUDE_MINUTE / 60 AS LATITUDE_END,
    -(FE_END_LONGITUDE_DEGREE + FE_END_LONGITUDE_MINUTE / 60) AS
      LONGITUDE_END,
      FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
    FROM FISHING_EVENT FE
    INNER JOIN TRIP T ON T.TRIP_ID = FE.TRIP_ID")


  .d <- inner_join(.d, unique(select(
    fe,
    FISHING_EVENT_ID,
    MONTH,
    DAY,
    TIME_DEPLOYED,
    TIME_RETRIEVED,
    LATITUDE,
    LONGITUDE,
    LATITUDE_END,
    LONGITUDE_END,
    DEPTH_M,
    VESSEL_ID,
    CAPTAIN_ID
  )),
  by = "FISHING_EVENT_ID")

  names(.d) <- tolower(names(.d))

  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)



  add_version(as_tibble(.d))
}
