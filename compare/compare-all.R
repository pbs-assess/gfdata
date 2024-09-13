# Compare specimens and then sets
{
  source(here::here("compare", "compare-survey-samples.R"))
  source(here::here("compare", "compare-survey-sets.R"))
}


library(tidyverse)

# Read samples files
xa <- readRDS("compare/results/samples-extras.rds")
ua <- readRDS("compare/results/samples-unlike.rds")
sa <- readRDS("compare/results/samples-summary.rds")
aa <- readRDS("compare/results/samples-alldiff.rds")

# Read sets files
xe <- readRDS("compare/results/sets-extras.rds")
ue <- readRDS("compare/results/sets-unlike.rds")
se <- readRDS("compare/results/sets-summary.rds")
ae <- readRDS("compare/results/sets-alldiff.rds")

# View -------------------------------------------------------------------------

# Samples
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

# Sets
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
