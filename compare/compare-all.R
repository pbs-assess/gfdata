# Compare specimens and then sets
{
  source(here::here("compare", "compare-survey-samples.R"))
  source(here::here("compare", "compare-survey-sets.R"))
}

# Load -------------------------------------------------------------------------

library(tidyverse)

# Read -------------------------------------------------------------------------

# Samples
xa <- readRDS("compare/results/samples-extras.rds")
ua <- readRDS("compare/results/samples-unlike.rds")
sa <- readRDS("compare/results/samples-summary.rds")
aa <- readRDS("compare/results/samples-alldiff.rds")

# Sets
xe <- readRDS("compare/results/sets-extras.rds")
ue <- readRDS("compare/results/sets-unlike.rds")
se <- readRDS("compare/results/sets-summary.rds")
ae <- readRDS("compare/results/sets-alldiff.rds")

# Samples ----------------------------------------------------------------------

# Extras

# Unlike
ua |>
  dplyr::distinct(
    dplyr::across(
      !dplyr::starts_with( # Ignore differences in these columns
        c(
          "fn",
          "month"
        )
      )
    ),
    .keep_all = TRUE
  ) |>
  dplyr::group_by(specimen_id) |>
  # Keep only groups with more than one row (the inconsistent groups)
  dplyr::filter(n() > 1) |>
  dplyr::ungroup() |>
  tibble::view()

# Summary
sa |>
  dplyr::group_by(species, ssid) |>
  dplyr::mutate(
    key_extras = ifelse(fn == 1, extra_ids, 0),
    key_view = max(key_extras, na.rm = TRUE) # Max within group
  ) |>
  dplyr::filter(key_view > 0) |>
  dplyr::ungroup() |>
  dplyr::select(-tidyselect::starts_with("key")) |>
  tibble::view()

# All diff


# Sets -------------------------------------------------------------------------

# Extras

# Unlike
ue |>
  dplyr::distinct(
    dplyr::across(
      !dplyr::starts_with( # Ignore differences in these columns
        c(
          "fn",
          "month",
          "speed",
          "area",
          "depth",
          "density"
        )
      )
    ),
    .keep_all = TRUE
  ) |>
  dplyr::group_by(comparison_id) |>
  # Keep only groups with more than one row (the inconsistent groups)
  dplyr::filter(n() > 1) |>
  dplyr::ungroup() |>
  tibble::view()

# Summary
se |>
  dplyr::group_by(species, ssid) |>
  dplyr::mutate(
    key_extras = ifelse(fn == 1, extra_ids, 0),
    key_view = max(key_extras, na.rm = TRUE) # Max within group
  ) |>
  dplyr::filter(key_view > 0) |>
  dplyr::ungroup() |>
  dplyr::select(-tidyselect::starts_with("key")) |>
  tibble::view()

# All diff
