# Overview
# - Write ssids to .rds file for easy use

# Write ssids ------------------------------------------------------------------

ssids <- gfdata::get_ssids() |>
  dplyr::filter(
    !(SURVEY_ABBREV == "OTHER" & !(SURVEY_SERIES_ID %in% c(9, 11, 68)))
  ) |>
  dplyr::filter(!grepl("EUL", SURVEY_ABBREV)) |>
  dplyr::filter(!grepl("Edas", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Salmon", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Ecosystem", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Commercial", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  # dplyr::filter(SURVEY_ABBREV != "SABLE") |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(ssid = survey_series_id)

saveRDS(ssids, file = "compare/data/ssids.rds")

# End --------------------------------------------------------------------------
