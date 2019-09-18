#' Environmental data extraction functions from surveys
#'
#' Environmental data extraction functions from surveys. See Details section.
#'
#' @details `get_sensor_data_trawl()`: Environmental data extraction for sensors
#' deployed on trawl survey gear
#'
#' @param ssid A numeric vector of survey series IDs. Run [get_ssids()] for a
#'   look-up table of available survey series IDs with surveys series
#'   descriptions.
#' @param attribute A character vector of sensor attributes to filter for. Run
#'   [get_sensor_attributes()] for a look-up table of available attributes.
#' @param spread_attributes Logical for whether the attributes should be
#'   returned in a wider format. Allows for user to choose whether data are
#'   output in wide format (`TRUE``) with min and max values for each attribute
#'   for each fishing event, or in long format (`FALSE``) with only mean values
#'   for each attribute and fishing event.
#'
#' @export
#' @rdname get_environmental_data
#'
#' @examples
#' d <- get_sensor_data_trawl(ssid = 1, "temperature")
#' head(d)
#' head(get_sensor_data_trawl_fe(d$fishing_event_id[[1]], "temperature"))
#'
#' d <- get_sensor_data_ll_td(ssid = 40)
#' head(d)
#' head(get_sensor_data_ll_td_fe(d$fishing_event_id[[1]],"temperature"))
#'
#' d <- get_sensor_data_ll_ctd(40)
#' head(d)

get_sensor_data_trawl <- function(ssid = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  spread_attributes = FALSE) {
  .q <- read_sql("get-sensor-data-trawl.sql")
  if (!is.null(ssid)) {
    .q <- inject_filter("AND SURVEY_SERIES_ID IN", ssid, .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d$attribute <- gsub("dissolved oxygen", "do", .d$attribute)
  .d <- dplyr::distinct(.d)
  .d <- .d %>%
    dplyr::mutate(attribute = paste0(attribute, "_", unit)) %>%
    dplyr::select(-unit)

  if (spread_attributes) {
    .d <- .d %>% tidyr::gather(.data$min, .data$avg, .data$max,
      key = "parameter", value = "value")
    .d <- .d %>% tidyr::unite(.data$temp, .data$attribute, .data$parameter)
    .d <- .d %>% tidyr::spread(key = .data$temp, value = .data$value)
  }

  tibble::as_tibble(.d)
}

#' @details
#' `get_sensor_data_trawl_fe()`: Environmental data extraction for trawl
#' surveys, for individual fishing events.
#'
#' @param fishing_event_id A vector of fishing events to filter for
#' @param sensor_name A character vector of sensor names to filter for.
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_trawl_fe <- function(fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  sensor_name = NULL){
  .q <- read_sql("get-sensor-data-trawl-fe.sql")
  if (is.null(fishing_event_id)) {
    stop("Please enter a fishing event id.")
  }
  .q <- inject_filter("AND FESD.FISHING_EVENT_ID IN", fishing_event_id, .q,
    search_flag = "-- insert fishing event id here", conversion_func = I
  )
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }
  if (!is.null(sensor_name)) {
    .q <- inject_filter("AND SENSOR_NAME IN", sensor_name, .q,
      search_flag = "-- insert sensor name here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d$attribute <- gsub("dissolved oxygen", "do", .d$attribute)
  .d <- dplyr::distinct(.d)
  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)
}

#' @details
#' `get_sensor_data_ll_td()`: Environmental data extraction for sensors
#' deployed on longline survey gear.
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_td <- function(ssid = NULL,
  attribute = c("temperature", "depth"),
  spread_attributes = FALSE) {
  .q <- read_sql("get-sensor-data-ll-td.sql")
  if (!is.null(ssid)) {
    .q <- inject_filter("AND SURVEY_SERIES_ID IN", ssid, .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d <- dplyr::distinct(.d)

  .d <- .d %>%
    mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)

  if (!spread_attributes) {
    .d <- .d %>% select(-min, -max)
    .d <- .d %>% tidyr::gather(avg, key = "parameter", value = "value") %>%
      select(-parameter)
    .d <- .d %>% tidyr::spread(key = attribute, value = value)
  }
  as_tibble(.d)
}

#' @details
#' `get_sensor_data_ll_td_fe()`: Environmental data extraction for sensors
#' deployed on longline survey gear for individual fishing events.
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_td_fe <- function(fishing_event_id = NULL,
  attribute = c("temperature", "depth")) {
  .q <- read_sql("get-sensor-data-ll-td-fe.sql")
  if (is.null(fishing_event_id)) {
    stop("Please enter a fishing event id.")
  }
  .q <- inject_filter("AND FE.FISHING_EVENT_ID IN", fishing_event_id, .q,
    search_flag = "-- insert fishing event id here", conversion_func = I
  )
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d$attribute <- gsub("dissolved oxygen", "do", .d$attribute)
  .d <- dplyr::distinct(.d)
  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)
}

#' @details
#' `get_sensor_data_ll_ctd()`: Environmental data extraction for ctd's deployed
#' on longline surveys. Ctd deployments are not tied directly to longline survey
#' fishing event id's. The unique fishing event id's from the deployments can be
#' linked to longline survey data by joining on the block designation (site
#' number).
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_ctd <- function(ssid = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  spread_attributes = FALSE) {
  .q <- read_sql("get-sensor-data-ll-ctd.sql")
  if (!is.null(ssid)) {
    .q <- inject_filter("AND SURVEY_SERIES_ID IN", ssid, .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d <- dplyr::distinct(.d)

  .d <- .d %>%
    mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)

  if (!spread_attributes) {
    .d <- .d %>% select(-min, -max)
    .d <- .d %>% tidyr::gather(avg, key = "parameter", value = "value") %>%
      select(-parameter)
    .d <- .d %>% tidyr::spread(key = attribute, value = value)
  }
  as_tibble(.d)
}

#' @details
#' `get_sensor_data_ll_ctd_fe()`: Envirnmental data extraction (ctd data) near
#' longline survey sites for individual fishing events.
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_ctd_fe <- function(fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity")){
  .q <- read_sql("get-sensor-data-ll-ctd-fe.sql")
  if (is.null(fishing_event_id)) {
    stop("Please enter a fishing event id.")
  }
  .q <- inject_filter("AND FE.FISHING_EVENT_ID IN", fishing_event_id, .q,
    search_flag = "-- insert fishing event id here", conversion_func = I
  )
  if (!is.null(attribute)) {
    .q <- inject_filter("AND SENSOR_DATA_ATTRIBUTE_DESC IN", first_cap(attribute), .q,
      search_flag = "-- insert attribute here", conversion_func = I
    )
  }

  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$attribute <- tolower(.d$attribute)
  .d$attribute <- gsub("dissolved oxygen", "do", .d$attribute)
  .d <- dplyr::distinct(.d)
  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)
}
