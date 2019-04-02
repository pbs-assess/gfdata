#' @param attribute A character vector of sensor attributes to filter for.
#'   Run `get_sensor_attributes()` for a look-up table of available attributes.
#' @param spread_attributes Logical for whether the attributes should be
#'   returned in a wider format.
#' @export
#' @rdname get_environmental_data
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
  .d <- unique(.d)
  .d <- .d %>%
    mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)

  if (spread_attributes) {
    .d <- .d %>% tidyr::gather(.data$min, .data$avg, .data$max,
      key = "parameter", value = "value")
    .d <- .d %>% tidyr::unite(.data$temp, .data$attribute, .data$parameter)
    .d <- .d %>% tidyr::spread(key = .data$temp, value = .data$value)
  }

  as_tibble(.d)
}

#' Title
#'
#' @details To explore sensor data for an individual fishing event
#' @param fishing_event_id A vector of fishing events to filter for
#' @param sensor_name A character vector of sensor names to filter for.
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_fe_trawl <- function(fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  sensor_name = NULL){
  .q <- read_sql("get-sensor-data-fe.sql")
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
  .d <- unique(.d)
  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)
}

#' Sensor data extraction by fishing event
#'
#' @param sensor_name TODO
#'
#' @export
#' @rdname get_environmental_data
get_sensor_data_fe_ctd <- function(fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  sensor_name = NULL){
  .q <- read_sql("get-sensor-data-fe-ctd.sql")
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
  .d <- unique(.d)
  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)
}

#' @param sensor_min_max Allows for user to choose whether data are output in
#'   wide format (= TRUE) with min and max values for each attribute for each
#'   fishing event, or in long format (= FALSE) with only mean values for each
#'   attribute and fishing event.
#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_td <- function(ssid = NULL,
  attribute = c("temperature", "depth"),
  sensor_min_max = FALSE) {
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
  .d <- unique(.d)

  .d <- .d %>%
    mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)

  if (!sensor_min_max) {
    .d <- .d %>% select(-min_value, -max_value)
    .d <- .d %>% tidyr::gather(avg_value, key = "parameter", value = "value")
    .d <- .d %>% tidyr::spread(key = attribute, value = value)
  }
  as_tibble(.d)
}

#' @export
#' @rdname get_environmental_data
get_sensor_data_ll_td <- function(ssid = NULL,
  attribute = c("temperature", "depth"),
  sensor_min_max = FALSE){
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
  .d <- unique(.d)

  .d <- .d %>% mutate(attribute = paste0(attribute, "_", unit)) %>%
    select(-unit)

  if (!sensor_min_max){
    .d <- .d %>% select(-min_value, -max_value)
    .d <- .d %>% tidyr::gather(avg_value, key = "parameter", value = "value")
    .d <- .d %>% tidyr::spread(key = attribute, value = value)
  }
  as_tibble(.d)
}
