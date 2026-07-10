species_codes <- gfsynopsis::get_spp_names()$species_code
set_dat <- gfdata::get_all_survey_sets(
  species = .d$species_code,
  ssid = c(1, 3, 4, 16, 2, 7, 22, 36, 39, 40),
  remove_false_zeros = FALSE,
  usability = c(0, 1, 2, 6)
)

if (FALSE) {
  set_data <-
    readRDS("../gfsynopsis-2025/report/data-cache-2026-07/survey-sets.rds")
}

# Temporary fix for 2025 HBLL stratum codes:
hbll_out_lu <- structure(list(grouping_code = c(448, 449, 450, 451, 452, 453,
  454, 455, 456, 457, 458, 459), fe_grouping_code = c(321, 322,
    323, 321, 322, 323, 321, 322, 323, 321, 322, 323)), row.names = c(NA,
      -12L), class = "data.frame")

hbll_ins_lu <- structure(list(grouping_code = c(279, 280, 281, 282), fe_grouping_code = c(317,
  318, 317, 318)), row.names = c(NA, -4L), class = "data.frame")

set_data <- set_data |>
  dplyr::left_join(hbll_out_lu, by = "grouping_code") |>
  dplyr::mutate(
    grouping_code = dplyr::if_else(!is.na(fe_grouping_code), fe_grouping_code, grouping_code)
  ) |>
  dplyr::select(-fe_grouping_code) |>
  dplyr::group_by(survey_series_id, grouping_code) |>
  dplyr::mutate(
    grouping_area_km2 = ifelse(
      is.na(grouping_area_km2) & any(!is.na(grouping_area_km2)),
      unique(stats::na.omit(grouping_area_km2))[1],
      grouping_area_km2
    )
  ) |>
  dplyr::ungroup()

set_data <- set_data |>
  dplyr::left_join(hbll_ins_lu, by = "grouping_code") |>
  dplyr::mutate(
    grouping_code = dplyr::if_else(!is.na(fe_grouping_code), fe_grouping_code, grouping_code)
  ) |>
  dplyr::select(-fe_grouping_code) |>
  dplyr::group_by(survey_series_id, grouping_code) |>
  dplyr::mutate(
    grouping_area_km2 = ifelse(
      is.na(grouping_area_km2) & any(!is.na(grouping_area_km2)),
      unique(stats::na.omit(grouping_area_km2))[1],
      grouping_area_km2
    )
  ) |>
  dplyr::ungroup()

# Keep only the columns get_design_index()/boot_one_year_dt() actually use,
# and pre-split by survey_series_id, so each worker only ever receives the
# (much smaller) subset of set_data it needs rather than the whole ~2 GB
# object being exported as a shared global to every parallel worker.
needed_cols <- c(
  "species_common_name", "species_science_name",
  "survey_series_id", "survey_id", "survey_abbrev", "survey_series_desc",
  "grouping_code", "usability_code", "year",
  "density_kgpm2", "density_ppkm2", "grouping_area_km2"
)
set_data_by_ssid <- set_data |>
  dplyr::select(dplyr::all_of(needed_cols)) |>
  split(set_data$survey_series_id)

# 10 SSIDs, split in half:
future::plan(future::multisession, workers = 5L)

set.seed(123)
tictoc::tic()
indexes <- furrr::future_map(set_data_by_ssid, \(ssid_dat) {
  ssid <- ssid_dat$survey_series_id[1]
  spp_in_ssid <- sort(unique(ssid_dat$species_common_name))
  purrr::map_dfr(spp_in_ssid, \(sp) {
    cat(" SSID: ", ssid, sp, "\n")
    this_dat <- dplyr::filter(ssid_dat, species_common_name == sp)
    out <- gfdata::get_design_index(sp, ssid = ssid, reps = 20000L, data = this_dat)
    out$species_common_name <- sp
    out$survey_series_id <- ssid
    out
  })
}, .options = furrr::furrr_options(seed = TRUE)) |>
  dplyr::bind_rows()
tictoc::toc()

future::plan(future::sequential)

names(indexes)

design_indexes <- dplyr::select(indexes, survey_abbrev, species_common_name, year, biomass, lowerci, upperci, cv_boot, cv_design)
design_indexes <- dplyr::mutate(design_indexes, dplyr::across(c(biomass, lowerci, upperci, cv_boot, cv_design), \(x) round(x, 4L)))

usethis::use_data(design_indexes, overwrite = TRUE)
