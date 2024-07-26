# Overview
# - Script iterates over species and ssids
# - Compares specimen_id column for extras in each data frame
# - Identifies rows indexes for extra specimen_id values
# - Accumulates extra specimen_id rows for each iteration
#
# Wish list
# - Update get_survey_samples*() functions to avoid errors when no data
# - E.g. spp <- "009"; ssid <- 1; returns errors for both functions

# Get ssids - Gives a pretty reasonable set
ids <- gfdata::get_ssids() |>
  dplyr::filter(!grepl("OTHER", SURVEY_ABBREV)) |>
  dplyr::filter(!grepl("EUL", SURVEY_ABBREV)) |>
  dplyr::filter(!grepl("Edas", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Salmon", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Ecosystem", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Commercial", SURVEY_SERIES_DESC, ignore.case = TRUE)) |>
  dplyr::pull(SURVEY_SERIES_ID)

# Get species - way way too many
# TODO: Refine list much much further or take from different source
spp <- gfdata::get_species() |>
  dplyr::filter(taxonomic_rank == "species") |>
  dplyr::filter(species_grouping == "fish") |>
  dplyr::filter(!is.na(species_common_name)) |>
  dplyr::filter(!grepl("Salmon", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Lamprey", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Eel", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Shark", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Stingray", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Sturgeon", species_common_name, ignore.case = TRUE)) |>
  dplyr::filter(!grepl("Anchovy", species_common_name, ignore.case = TRUE)) |>
  dplyr::pull(species_code)

# Shorter species set
spp <- "225" # Test with Hake - works okay

# Initialize storage tibbles
s1 <- tibble::tibble()
s2 <- tibble::tibble()

# Iterate over cases
for (i in seq_along(spp)) {
  for (j in seq_along(ids)) {
    # Pull data
    d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ids[j])
    d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ids[j])
    # Identify extra specimen_id
    n1 <- setdiff(d1$specimen_id, d2$specimen_id)
    n2 <- setdiff(d2$specimen_id, d1$specimen_id)
    # Identify extra specimen_id rows
    r1 <- which(d1$specimen_id %in% n1)
    r2 <- which(d2$specimen_id %in% n2)
    # Store extra specimen_id rows
    s1 <- rbind(s1, tibble::tibble(spp = spp[i], ssid = ids[j], d1[r1, ]))
    s2 <- rbind(s2, tibble::tibble(spp = spp[i], ssid = ids[j], d2[r2, ]))
  }
}
