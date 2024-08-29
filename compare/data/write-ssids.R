# Overview
# - Write ssids to .rds file for easy use

# Write ssids ------------------------------------------------------------------

# Gives data for 26 ssids
# ssids <- gfdata::get_ssids() |>
#   dplyr::filter(
#     !(SURVEY_ABBREV == "OTHER" & !(SURVEY_SERIES_ID %in% c(9, 11, 68)))
#   ) |>
#   dplyr::filter(!grepl("EUL", SURVEY_ABBREV)) |>
#   dplyr::filter(!grepl("Edas", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
#   dplyr::filter(!grepl("Salmon", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
#   dplyr::filter(!grepl("Ecosystem", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
#   dplyr::filter(!grepl("Commercial", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
#   # dplyr::filter(SURVEY_ABBREV != "SABLE") |>
#   dplyr::rename_with(tolower) |>
#   dplyr::rename(ssid = survey_series_id)

# Gives numbers for 43 ssids from Philina's list
ssid_nums <- tibble::tibble(
  ssid = c(
    # OUTSIDE
    1,
    2, 3,
    4,
    5,
    6, 7, 8, 9, 10, 11,
    14,
    22, 36, # HBLL
    25, # Thornyhead
    21, 26,
    35,
    46,
    49, 50, 51,
    67, 68,
    81,
    94,
    # INSIDE
    45, # SOG trawl
    20, # Synoptic Vancouver region?
    79, # Triennial Vancouver region
    82:87, # Jig 4B
    13, # Inside longline?
    15, # SOG lingcod
    39, 40, # HBLL
    48, 93, 76, 92
  )
)

# Join numbers to ssid data
ssids <- gfdata::get_ssids() |>
  dplyr::rename_with(tolower) |>
  dplyr::rename(ssid = survey_series_id) |>
  dplyr::right_join(ssid_nums, by = "ssid")

# Write
saveRDS(ssids, file = "compare/data/ssids.rds")

# End --------------------------------------------------------------------------
