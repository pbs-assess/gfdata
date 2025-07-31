#' Multispecies Small-Mesh Bottom Trawl (MSSM) Survey Grid
#'
#' A 3x3 km grid for the Multispecies Small-Mesh Bottom Trawl Survey (MSSM; formerly
#' known as 'shrimp survey'). This grid covers WCVI Shrimp Survey Areas 124 and 125.
#' The `year` is the last year a grid cell was sampled as of 2023, with
#' the most consistent resampling (spatially) occurring in grid cells last sampled
#' between 2009 and 2021.
#'
#' @format ## `mssm_grid`
#' A data frame with 3,735 rows and 5 columns:
#' \describe{
#'   \item{survey_abbrev}{Survey abbreviation}
#'   \item{longitude, latitude}{Longitude and latitude of the centroid of 3x3 km
#'     grid cells}
#'   \item{area}{Area of the grid cells, in km^2}
#'   \item{year}{The year a grid cell was sampled. A grid cell can have multiple year values}
#' }
#'
#' @format ## `mssm_grid_sf`
#' A simple features (`sf` object) version of `mssm_grid`
"mssm_grid"

#' @rdname mssm_grid
"mssm_grid_sf"

#' IPHC data cleaned for use for non-halibut index standardization at PBS
#'
#' These data are best used for spatiotemporal analyses. Also see the
#' \pkg{gfiphc} package which implements a design-based index standardization.
#' Data from 1998 onwards come from the IPHC website.
#' Data from 1996 and 1997 come from local spreadsheets as described in
#' \pkg{gfiphc}.
#'
#' @format Once the 2 data frames are joined, a data frame with these columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{station}{Station ID}
#'   \item{station_key}{A unique station key from IPHC; occasionally there are multiple sets at the same `station` but this column is always unique. It's also unique across years.}
#'   \item{longitude}{Longitude (mid point)}
#'   \item{latitude}{Latitude (mid point)}
#'   \item{species_science_name}{Scientific name}
#'   \item{hooks_observed}{Number of hooks observed for non-halibut species}
#'   \item{number_observed}{Number of hooks with the species of interest}
#'   \item{pbs_standard_grid}{Logical: standard grid stations through time as defined in gfiphc}
#'   \item{inside_wcvi}{Logical: inside Vancouver Island waters (2018 only) vs. anywhere else; you may want to exclude these from spatiotemporal modelling}
#'   \item{sample_type}{Sample type (first 20 hooks vs. all hooks)}
#'   \item{soak_time_min}{Soak time}
#'   \item{temp_c}{Temperature in degrees C}
#'   \item{depth_m}{Depth in m}
#'   \item{species_common_name}{Species common name}
#' }
#'
#' @details
#' One likely wants to join the data frames. E.g.
#'
#' ```
#' iphc <- dplyr::inner_join(iphc_catch, iphc_sets)
#' ```
#'
#' @rdname iphc
"iphc_sets"

#' @rdname iphc
"iphc_catch"

#' Active survey blocks
#'
#' Active survey blocks for DFO Pacific groundfish surveys.
#' Obtained via `gfdata::get_active_survey_blocks()` with some cleaning
#' as documented in `data-raw/survey_blocks.R`.
#'
#' @format Simple feature (`sf`) collection with 66744 features and 8 fields:
#' \describe{
#'   \item{survey_abbrev}{Survey abbreviation.}
#'   \item{survey_series_id}{Unique identifier for the survey series.}
#'   \item{block_id}{Unique identifier for each grid cell (`BLOCK_DESIGNATION` in GFBioSQL).}
#'   \item{grouping_code}{Strata grouping code used to join with strata data from
#'         the GROUPING table in GFBioSQL.}
#'   \item{depth_m}{Depth in metres.}
#'   \item{active_block}{Is block actively fished as of date downloaded:
#'      e.g., `attr(gfdata::survey_blocks, "date-downloaded")`)}
#'  \item{geometry}{Represents grid cell.}
#'  \item{area}{Overwater area in km^2.}
#'}
#'
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' requireNamespace("ggplot2", quietly = TRUE)
#' library(sf)
#' library(ggplot2)
#' gfdata::survey_blocks |>
#'   dplyr::filter(active_block) |>
#'   ggplot(aes(colour = survey_abbrev)) +
#'   geom_sf() +
#'   theme_minimal() +
#'   scale_colour_brewer(palette = "Dark2")
#' attr(gfdata::survey_blocks, "date-generated")
#' attr(gfdata::survey_blocks, "date-downloaded")
#'
"survey_blocks"
