
library(tidyverse)
devtools::load_all(".")

source(here::here("compare", "R", "functions-sets.R"))
source(here::here("compare", "R", "functions-samples.R"))

# spp <- "pink shrimp (smooth)"
# ds <- compare_survey_sets(spp = spp, ssid = c(2))
# xs2 <- ds$x |> group_by(fn, ssid, species, survey_abbrev, major_stat_area_code) |>
#   summarise(n = n())
# # either function works, original misses only 2 sets both 0s due to grouping_code differences
#
# dd <- ds$x[duplicated(ds$x$fishing_event_id),]
# # no duplicates

# ## Inside
# ssid <- c(
#   45, #SOG trawl
#   20, # synoptic vancouver region?
#   79, # triennial vancouver region
#   82-87, #Jig 4B
#   13, # inside longline?
#   15, # SOG lingcod
#   39, 40, 48, 93, 76, 92)
# spp <- "North Pacific Spiny Dogfish"
#
# ## Outside
#
# maj <- get_major_areas()
# ssid <- c(0,
#           #1,
#           2,3,
#           #4,
#           5,
#           #6,7, 8, 9, 10, 11,
#           14,
#           22,
#           # 25, # thornyhead
#           21, 26,
#           35, 36,
#           # 46, 48,
#           49, 50, 51,
#           67, 68,
#           81, 94)
#
ssid <- c(
  #OUTSIDE
  1,
  2,3,
  4,
  5,
  6,7, 8, 9, 10, 11,
  14,
  22, 36, # HBLL
  25, # thornyhead
  21, 26,
  35,
  46,
  49, 50, 51,
  67, 68,
  81,
  94,
  #INSIDE
  45, #SOG trawl
  20, # synoptic vancouver region?
  79, # triennial vancouver region
  82-87, #Jig 4B
  13, # inside longline?
  15, # SOG lingcod
  39, 40, # HBLL
  48, 93, 76, 92
  )

spp <- c("Lingcod")
spp <- c("Yelloweye Rockfish")

spp <- c("394")
# major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09", "11") # unknown "0","00",
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99")
#
# dset <- compare_survey_sets(spp = spp, ssid <- c(39, 40, 48, 93, 76, 92))
# dset <- compare_survey_sets(spp = spp, ssid = c(35, 41, 42, 43))
# dset <- compare_survey_sets(spp = spp, ssid = c(6,7))
#
dset <- compare_survey_sets(spp = spp, ssid = ssid)

saveRDS(dset, "test-yelloweye-sets-all-0826.rds")

dsam11 <- compare_survey_samples(spp = spp, ssid = ssid, areas = major_areas)

saveRDS(dsam, "test-rougheye-samples-all-0827.rds")


saveRDS(dsam, "test-lingcod-samples-all-0826.rds")
saveRDS(dsam, "test-yelloweye-samples-all-0826.rds")
# saveRDS(dsam, "test-yelloweye-samples-incomplete.rds")


#
# ## INSIDE DOGFISH TEST
# spp <- "North Pacific Spiny Dogfish"
#
# # x <-get_all_survey_samples(species = spp,
# #                        ssid = NULL,
# #                        major = c("01","68"), # SOG and Puget Sound,
# #                        unsorted_only = FALSE, random_only = FALSE,
# #                        remove_duplicates = TRUE)
# # ssid <- unique(x$survey_series_id)
# # [1] 85 82 15 39 40 48 50 34 51 45 68 76 86 87
# dsam <- compare_survey_samples(spp = spp, ssid = ssid)
#
# saveRDS(dsam, "test-dogfish-inside-samples-all.rds")
#
#
# dsam <- readRDS("test-yelloweye-samples-all.rds")

# dsam2 is the default get_all setting
# dsam is the most conservative

nrow(dsam$x)

nrow(dsam2$x)
nrow(dsam11$x)


x <- dsam11$x

# x1 <- dsam$x |>
#   filter((major_stat_area_code %in% c("01","68")))
#
# x2 <- dsam$x |>
#   filter(!(major_stat_area_code %in% c("01","68")))
#
# check <- x |> filter(fn == 2)


xx1 <- x  |>
  group_by(ssid,fn, species, survey_series_desc) |>
  mutate(age_years = ifelse(!is.na(age), year, NA)) |>
  summarise(n = n(),
            lengths = length(na.omit(length)),
            weights = length(na.omit(weight)),
            ages = length(na.omit(age)),
            maturities = sum(na.omit(maturity_code) != 9),
            unique_fe = length(unique(fishing_event_id)),
            min_age_year = min(age_years, na.rm = TRUE),
            max_age_year = max(age_years, na.rm = TRUE)
            )


xx3 <- d2  |>
  group_by(survey_series_id, survey_series_desc) |>
  mutate(age_years = ifelse(!is.na(age), year, NA)) |>
  summarise(n = n(),
            lengths = length(na.omit(length)),
            weights = length(na.omit(weight)),
            ages = length(na.omit(age)),
            maturities = sum(na.omit(maturity_code) != 9),
            unique_fe = length(unique(fishing_event_id)),
            min_age_year = min(age_years, na.rm = TRUE),
            max_age_year = max(age_years, na.rm = TRUE)
  )



dd <- x[duplicated(x$specimen_id),]
dd2 <- filter(x, (specimen_id %in% c(dd$specimen_id)))


xx3 <- dd2 |>
  group_by(fn, ssid, species, survey_series_id, major_stat_area_code
           #, usability_desc
           ) |>
  summarise(n = n())

xx4 <- d_feg$x |>
  group_by(fn, ssid, species, survey_abbrev, major_stat_area_code, year, usability_desc) |>
  summarise(n = n())



check <- dset$x |> filter(fn == 2 & ssid == 7)

# Unlike values
u <- dset$u
u <- dsam$u

# Summary of returns
s <- dset$s
s <- dsam$s
