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

#' Active survey blocks
#'
#' Active survey blocks for DFO Pacific groundfish surveys.
#' Obtained via `gfdata::get_active_survey_blocks()` with some cleaning
#' as documented in `data-raw/survey_blocks.R`.
"survey_blocks"
