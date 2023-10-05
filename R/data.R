#' Multispecies Small-Mesh Bottom Trawl (MSSM) Survey Grid
#'
#' A 2x2 km grid for the Multispecies Small-Mesh Bottom Trawl Survey (MSSM; formerly
#' known as 'shrimp survey'). This grid covers WCVI Shrimp Survey Areas 124 and 125.
#' The `last_samp_year is the last year a grid cell was sampled as of 2023, with
#' the most consistent resampling (spatially) occurring in grid cells last sampled
#' after 2008 (GFBioField used used starting in 2009).
#'
#' @format ## `mssm_grid`
#' A data frame with 493 rows and 5 columns:
#' \describe{
#'   \item{survey_abbrev}{Survey abbreviation}
#'   \item{longitude, latitude}{Longitude and latitude of the centroid of 2x2 km
#'     grid cells}
#'   \item{area}{Area of the grid cells, in km^2}
#'   \item{last_samp_year}{The last year a grid cell was sampled as of the date
#'     this dataset was created (Oct 2023)}
#' }
"mssm_grid"