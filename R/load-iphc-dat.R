#' Load and join IPHC FISS set and catch data
#'
#' These data start with year 1996.
#'
#' @return A tibble with 273,714 rows and 22 columns, containing the processed IPHC catch and set data.
#' The main columns include:
#' \itemize{
#'   \item year. Year the set was hauled.
#'   \item station. Permanent station ID named by IPHC.
#'   \item station_key. Unique key for the set; can be used to join set table to non-Pacific halibut catch data from FISS Survey download page. Occasionally there are multiple sets at the same \code{station} but this column is always unique. It's also unique across years.
#'   \item species_common_name. Species name used in GFBio.
#'   \item species_science_name. Scientific name used in GFBio.
#'   \item number_observed. Number of hooks with the species of interest. Some species such as skates and shortspine thornyheads were not enumerated to the species level in early years and so these values are `NA`.
#'   \item longitude. Longitude in decimal degrees of the midpoint of the set.
#'   \item latitude. Latitude in decimal degrees of the midpoint of the set.
#'   \item usable. IPHC indication of whether station was deemed effective ("Y") or ineffective ("N") for assessment.
#'   \item hooks_retrieved. Number of hooks retrieved.
#'   \item hooks_observed. Number of hooks observed for non-Pacific halibut species catch.
#'   \item pbs_standard_grid. Stations defined as 'standard' in `gfiphc`.
#'   \item inside_wcvi. Logical: inside Vancouver Island waters (2018 only) vs. anywhere else; you may want to exclude these from spatiotemporal modelling.
#'   \item sample_type. Type of observations.
#'   * "20 hooks" - Observation of the first 20 (non-halibut) hooks of each skate.
#'   * "all hooks" - All hooks observed.
#'
#'   \item depth_m. Average of beginning and end depth of set in metres.
#'   \item temp_c. Temperature at profiler max pressure (degrees Celsius).
#'   \item soak_time_min. Time interval gear was in water (minutes).
#'   \item avg_no_hook_per_skate. Average number of hooks per skate at setting.
#'   \item no_skates_hauled. Number of skates hauled (no metadata on IPHC website).
#'   \item no_skates_set. Number of skates set, not adjusted for baits or average number of hooks per skate.
#'   \item effective_skates. "An effective skate is 100 baited hooks. The average number of hooks/skate and the number of missing baits at setting factor into the effective skate calculation". Description from IPHC table: "data field names defined"
#'   \item baits_returned. The number of baited hooks remaining. These are unavailable (NA) for some Pacific halibut records where the sample_type = '20 hooks'.
#'}
#'
#' The tibble also contains the following attributes:
#' \itemize{
#'   \item iphc_download_date. Date when the data was downloaded, e.g., "2024-05-23".
#'   \item data_preparation_date. Date  the data was prepared, e.g., "2024-06-11".
#' }
#'
#' @details This function joins the IPHC catch data with the set metadata.
#' This is needed because Pacific halibut are enumerated for all hooks_retrieved
#' in a year but non-halibut species are only enumerated for hooks_observed in a
#' year. The catch and set dataframes are saved separately for space efficiency.
#'
#' @examples
#' \dontrun{
#' # Retrieve and process the IPHC data
#' iphc_data <- load_iphc_dat()
#' }
#'
#' @export
load_iphc_dat <- function() {
  iphc_dat <- dplyr::inner_join(gfdata::iphc_catch, gfdata::iphc_sets,
                                by = c("year", "station", "station_key")) |>
    dplyr::mutate(baits_returned = dplyr::case_when(
      .data$species_common_name == "pacific halibut" & .data$sample_type == "all hooks" ~ .data$baits_returned,
      .data$species_common_name == "pacific halibut" & .data$sample_type == "20 hooks" ~ NA, # we do not have the data to know returned baited hooks for halibut catch
      .data$species_common_name != "pacific halibut" ~ .data$baits_returned
    )) |>
    dplyr::mutate(hooks_observed = dplyr::case_when(
      .data$species_common_name == "pacific halibut" & .data$sample_type == "all hooks" ~ .data$hooks_observed,
      .data$species_common_name == "pacific halibut" & .data$sample_type == "20 hooks" ~ .data$hooks_retrieved,
      .default = .data$hooks_observed
    ))
}