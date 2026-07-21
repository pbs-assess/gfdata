# temporary script for 2025 because the other species were run first
salmon_species <- c(
  "chinook salmon", "chum salmon", "coho salmon", "pink salmon", "sockeye salmon"
)

cache_dir <- "../gfsynopsis-2025/report/data-cache-2026-07/survey-sets"
salmon_files <- file.path(cache_dir, paste0(gsub(" ", "-", salmon_species), ".rds"))

set_data <- purrr::map_dfr(salmon_files, readRDS) |>
  dplyr::rename(grouping_area_km2 = area_km2)

# These cached files were already filtered to usable records and do not retain
# the usability column. Code 0 is one of the accepted usability codes.
set_data$usability_code <- 0L

# Apply the same temporary 2025 HBLL stratum-code corrections as the full run.
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
  dplyr::ungroup() |>
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

needed_cols <- c(
  "species_common_name", "species_science_name",
  "survey_series_id", "survey_id", "survey_abbrev", "survey_series_desc",
  "grouping_code", "usability_code", "year",
  "density_kgpm2", "density_ppkm2", "grouping_area_km2"
)
set_data_by_ssid <- set_data |>
  dplyr::select(dplyr::all_of(needed_cols)) |>
  split(set_data$survey_series_id)

future::plan(future::multisession, workers = 5L)
set.seed(123)
tictoc::tic()
new_design_indexes <- furrr::future_map(set_data_by_ssid, \(ssid_dat) {
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
  dplyr::bind_rows() |>
  dplyr::select(survey_abbrev, species_common_name, year, biomass, lowerci, upperci, cv_boot, cv_design) |>
  dplyr::mutate(dplyr::across(c(biomass, lowerci, upperci, cv_boot, cv_design), \(x) round(x, 4L)))
tictoc::toc()
future::plan(future::sequential)

existing_design_indexes <- new.env(parent = emptyenv())
load("data/design_indexes.rda", envir = existing_design_indexes)
design_indexes <- get("design_indexes", envir = existing_design_indexes) |>
  dplyr::filter(!species_common_name %in% salmon_species) |>
  dplyr::bind_rows(new_design_indexes)

usethis::use_data(design_indexes, overwrite = TRUE)
