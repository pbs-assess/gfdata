# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")

# # Get grouping codes --------------------------------------------------------------------
# sg <- get_table("Survey_Grouping")
# sid <- get_table("Survey")
#
# sg2 <- left_join(sg, sid, by = "SURVEY_ID") |> rename_with(tolower) |> group_by(survey_series_id) |>
#   summarise(
#     grouping_codes = as.character(paste0(unique(grouping_code), collapse = ", "))
#     )
#
# saveRDS(sg2, "compare/data/grouping_codes.rds")
#
# # Get usability codes --------------------------------------------------------------------
# u <- get_table("Usability")
# saveRDS(u, "compare/data/usability_codes.rds")

# Set species of interest ---------------------------------------------------------------
spp <- c("Quillback Rockfish", "Yelloweye Rockfish", "Lingcod", "North Pacific Spiny Dogfish")
ids <- c(6,7,67)

spp <- c("North Pacific Spiny Dogfish")
ids <- c(48,76,92,93)

# if used together as intended and much faster!
# # Initialize storage tibbles
# s1 <- tibble::tibble()
# s2 <- tibble::tibble()
#
# d1 <- tibble::tibble()
# d2 <- tibble::tibble()
# # Pull data
# try(d1 <- gfdata::get_survey_samples2(species = spp, ssid = ids))
# try(d2 <- gfdata::get_survey_samples2(species = spp, ssid = ids, include_activity_matches = TRUE))
# # Identify extra specimen_id
# n1 <- setdiff(d1$specimen_id, d2$specimen_id)
# n2 <- setdiff(d2$specimen_id, d1$specimen_id)
# # Identify extra specimen_id rows
# r1 <- which(d1$specimen_id %in% n1)
# r2 <- which(d2$specimen_id %in% n2)
# # Store extra specimen_id rows
# s1 <- rbind(s1, tibble::tibble(ssid = ids[j], d1[r1, ]))
# s2 <- rbind(s2, tibble::tibble(ssid = ids[j], d2[r2, ]))
# # result is identical for both with ssids 6,7,67
# saveRDS(d1, "compare/data/ssid-based-records-smms.rds")
# saveRDS(d2, "compare/data/activity-based-records-smms.rds")

# # Iterate over cases
# s1 <- tibble::tibble()
# s2 <- tibble::tibble()
# for (i in seq_along(spp)) {
# for (j in seq_along(ids)) {
#     d1 <- tibble::tibble()
#     d2 <- tibble::tibble()
#     # Pull data
#     try(d1 <- gfdata::get_survey_samples2(species = spp[i], ssid = ids[j]))
#     try(d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ids[j], include_activity_matches = TRUE))
#     # Identify extra specimen_id
#     n1 <- setdiff(d1$specimen_id, d2$specimen_id)
#     n2 <- setdiff(d2$specimen_id, d1$specimen_id)
#     # Identify extra specimen_id rows
#     r1 <- which(d1$specimen_id %in% n1)
#     r2 <- which(d2$specimen_id %in% n2)
#     # Store extra specimen_id rows
#     s1 <- rbind(s1, tibble::tibble(ssid = ids[j], d1[r1, ]))
#     s2 <- rbind(s2, tibble::tibble(ssid = ids[j], d2[r2, ]))
#   }
# }
#
# # saveRDS(s1, "compare/data/activity-based-records-missed-smms.rds")
# # saveRDS(s2, "compare/data/activity-based-records-added-smms.rds") # when iterated
# s1 <- readRDS("compare/data/activity-based-records-missed-smms.rds")
# s2 <- readRDS("compare/data/activity-based-records-added-smms.rds")


# Iterate over species for original only

d1 <- d2 <- d3 <- tibble::tibble()
n1 <- n2 <- n3 <- n4 <- NULL

s1 <- s2 <- s3 <- s4 <- tibble::tibble()

# Pull data
for (i in seq_along(spp)) {
  d1i <- tibble::tibble()
  try(d1i <- gfdata::get_survey_samples(species = spp[i], ssid = ids))
  d1 <- rbind(d1, tibble::tibble(d1i))
}
try(d2 <- gfdata::get_survey_samples2(species = spp, ssid = ids))
try(d3 <- gfdata::get_survey_samples2(species = spp, ssid = ids, include_activity_matches = TRUE))

#
# saveRDS(d1, "compare/data/d1-original-records-smms.rds")
# saveRDS(d2, "compare/data/d2-ssid-based-records-smms.rds")
# saveRDS(d3, "compare/data/d3-activity-based-records-smms.rds")

saveRDS(d1, "compare/data/d1-original-records-dog.rds")
saveRDS(d2, "compare/data/d2-ssid-based-records-dog.rds")
saveRDS(d3, "compare/data/d3-activity-based-records-dog.rds")

# Identify extra specimen_id
n1 <- setdiff(d1$specimen_id, d2$specimen_id) # in function 1 relative to function 2
n2 <- setdiff(d2$specimen_id, d1$specimen_id) # in function 2 relative to function 1
n3 <- setdiff(d3$specimen_id, d1$specimen_id) # in function 2 w activity vs function 1
n4 <- setdiff(d3$specimen_id, d2$specimen_id) # in function 2 w activity vs without
# Store extra specimen_id rows
s1 <- d1[which(d1$specimen_id %in% n1), ]
s2 <- d2[which(d2$specimen_id %in% n2), ]
s3 <- d3[which(d3$specimen_id %in% n3), ]
s4 <- d3[which(d3$specimen_id %in% n4), ]

# saveRDS(s1, "compare/data/activity-based-records-missed-smms.rds")
# saveRDS(s2, "compare/data/activity-based-records-added-smms.rds") # when iterated



# remove duplicated specimens
dd <- d2[duplicated(d2$specimen_id),]
dd1 <- filter(d2, !(specimen_id %in% c(dd$specimen_id)))
dd2 <- filter(d2, (specimen_id %in% c(unique(dd$specimen_id)) #& !(survey_abbrev %in% c("MSSM WCVI", "DOG"))
                   ))

# # confirm that it worked
# s2b <- bind_rows(dd1, dd2)
# dd2 <- s2b[duplicated(s2b$specimen_id),]
# unique(dd2$survey_series_id)


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
