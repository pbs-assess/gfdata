% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-envir-dat.R
\name{get_sensor_data_trawl}
\alias{get_sensor_data_trawl}
\alias{get_sensor_data_trawl_fe}
\alias{get_sensor_data_ll_td}
\alias{get_sensor_data_ll_td_fe}
\alias{get_sensor_data_ll_ctd}
\alias{get_sensor_data_ll_ctd_fe}
\title{Environmental data extraction functions from surveys}
\usage{
get_sensor_data_trawl(
  ssid = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  spread_attributes = FALSE
)

get_sensor_data_trawl_fe(
  fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  sensor_name = NULL
)

get_sensor_data_ll_td(
  ssid = NULL,
  attribute = c("temperature", "depth"),
  spread_attributes = FALSE
)

get_sensor_data_ll_td_fe(
  fishing_event_id = NULL,
  attribute = c("temperature", "depth")
)

get_sensor_data_ll_ctd(
  ssid = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity"),
  spread_attributes = FALSE
)

get_sensor_data_ll_ctd_fe(
  fishing_event_id = NULL,
  attribute = c("temperature", "depth", "dissolved oxygen", "salinity")
)
}
\arguments{
\item{ssid}{A numeric vector of survey series IDs. Run \code{\link[=get_ssids]{get_ssids()}} for a
look-up table of available survey series IDs with surveys series
descriptions.}

\item{attribute}{A character vector of sensor attributes to filter for. Run
\code{\link[=get_sensor_attributes]{get_sensor_attributes()}} for a look-up table of available attributes.}

\item{spread_attributes}{Logical for whether the attributes should be
returned in a wider format. Allows for user to choose whether data are
output in wide format (\verb{TRUE``) with min and max values for each attribute for each fishing event, or in long format (}FALSE``) with only mean values
for each attribute and fishing event.}

\item{fishing_event_id}{A vector of fishing events to filter for}

\item{sensor_name}{A character vector of sensor names to filter for.}
}
\description{
Environmental data extraction functions from surveys. See Details section.
}
\details{
\code{get_sensor_data_trawl()}: Environmental data extraction for sensors
deployed on trawl survey gear

\code{get_sensor_data_trawl_fe()}: Environmental data extraction for trawl
surveys, for individual fishing events.

\code{get_sensor_data_ll_td()}: Environmental data extraction for sensors
deployed on longline survey gear.

\code{get_sensor_data_ll_td_fe()}: Environmental data extraction for sensors
deployed on longline survey gear for individual fishing events.

\code{get_sensor_data_ll_ctd()}: Environmental data extraction for ctd's deployed
on longline surveys. Ctd deployments are not tied directly to longline survey
fishing event id's. The unique fishing event id's from the deployments can be
linked to longline survey data by joining on the block designation (site
number).

\code{get_sensor_data_ll_ctd_fe()}: Envirnmental data extraction (ctd data) near
longline survey sites for individual fishing events.
}
\examples{
\dontrun{
d <- get_sensor_data_trawl(ssid = 1, "temperature")
head(d)
head(get_sensor_data_trawl_fe(d$fishing_event_id[[1]], "temperature"))

d <- get_sensor_data_ll_td(ssid = 40)
head(d)
head(get_sensor_data_ll_td_fe(d$fishing_event_id[[1]],"temperature"))

d <- get_sensor_data_ll_ctd(40)
head(d)
}
}
