# Overview
# - Save species names and ssids as .rds files for easy use

# Save species -----------------------------------------------------------------

species_names <- c(
  "North Pacific Spiny Dogfish",
  "Pacific Ocean Perch",
  "Pacific Cod",
  "Walleye Pollock",
  "Sablefish",
  "Lingcod",
  "Bocaccio",
  "Canary Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Silvergray Rockfish",
  "Shortspine Thornyhead",
  "Widow Rockfish",
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish",
  "Copper Rockfish",
  "Shortraker Rockfish",
  "Rosethorn Rockfish",
  "Harlequin Rockfish",
  "Pygmy Rockfish",
  "Sharpchin Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Petrale Sole",
  "Arrowtooth Flounder",
  "English Sole",
  "Dover Sole",
  "Rex Sole",
  "Flathead Sole",
  "Southern Rock Sole",
  "Slender Sole",
  "Pacific Sanddab",
  "Pacific Halibut",
  "Butter Sole",
  "Pacific Hake",
  "Pacific Tomcod",
  "Spotted Ratfish",
  "Longnose Skate",
  "Big Skate",
  "Sandpaper Skate",
  "Curlfin Sole",
  "Sand Sole"
)

saveRDS(species_names, file = "compare/data/species-names.rds")

# Save ssids -------------------------------------------------------------------

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
