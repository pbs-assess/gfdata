# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")

# Get ssids --------------------------------------------------------------------

ssids <- readRDS("compare/data/ssids.rds")
ids <- ssids |>
  # filter(!(survey_abbrev %in% c("SABLE"))) |>
  filter(!(survey_abbrev %in% c("SABLE INLET", "SABLE OFF", "SABLE RAND"))) |>
  pull(ssid)

# Get grouping codes --------------------------------------------------------------------
sg <- get_table("Survey_Grouping")
sid <- get_table("Survey")

sg2 <- left_join(sg, sid, by = "SURVEY_ID") |> rename_with(tolower) |> group_by(survey_series_id) |>
  summarise(
    grouping_codes = as.character(paste0(unique(grouping_code), collapse = ", "))
    )

saveRDS(sg2, "compare/data/grouping_codes.rds")

# Get usability codes --------------------------------------------------------------------
u <- get_table("Usability")
saveRDS(u, "compare/data/usability_codes.rds")

# Set species of interest ---------------------------------------------------------------
spp <- c("Quillback Rockfish", "Yelloweye Rockfish", "Lingcod", "North Pacific Spiny Dogfish")

# Initialize storage tibbles
s1 <- tibble::tibble()
s2 <- tibble::tibble()

# Iterate over cases
for (i in seq_along(spp)) {
  for (j in seq_along(ids)) {
    d1 <- tibble::tibble()
    d2 <- tibble::tibble()
    # Pull data
    try(d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ids[j]))
    try(d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ids[j]))
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

# saveRDS(s2, "compare/data/missing-specimens-all-data.rds")
s2 <- readRDS("compare/data/missing-specimens-all-data2.rds")

# remove duplicated specimens
dd <- s2[duplicated(s2$specimen_id),]
dd1 <- filter(s2, !(specimen_id %in% c(dd$specimen_id)))
dd2 <- filter(s2, (specimen_id %in% c(dd$specimen_id)), !(survey_abbrev %in% c("MSSM WCVI", "DOG")))

# confirm that it worked
s2b <- bind_rows(dd1, dd2)
dd2 <- s2b[duplicated(s2b$specimen_id),]

# summarize what was missed
ss <- bind_rows(dd1, dd2) |>
  group_by(species_common_name, survey_series_desc, survey_series_id) |>
  summarise(
            lengths = sum(!is.na(length)),
            weights = sum(!is.na(weight)),
            ages = sum(!is.na(age)),
            years = as.character(paste0(unique(year), collapse = ", ")),
            records = n(),
            event_gc_not_expected = sum(is.na(survey_grouping_code)),
            missing_sample_gc = sum(is.na(sample_grouping_code)),
            missing_event_gc = sum(is.na(event_grouping_code)),
            grouping_codes = as.character(paste0(sort(unique(c(event_grouping_code, sample_grouping_code))), collapse = ", ")),
            usability_codes = as.character(paste0(unique(usability_code), collapse = ", ")),
            dominant_usability = names(which.max(table(usability_desc)))
            )

# ss <- left_join(ss, u) |> select(-usability_code)

sg2 <- readRDS("compare/data/grouping_codes.rds") |> rename(expected_gcs = grouping_codes)

ss2 <- left_join(ss, sg2) |> mutate(assigned_gcs = grouping_codes) |> select(-grouping_codes)
# note: cases where an assigned gc matches the expected, it was always from the gc assigned to the specimen, not the event.

saveRDS(ss2, "compare/data/missing-specimen-counts.rds")
write.csv(ss2, "compare/data/missing-specimen-counts.csv")

# ss <- readRDS("compare/data/missing-specimen-counts.rds")
