
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

## Inside
ssid <- c(
  45, #SOG trawl
  20, # synoptic vancouver region?
  79, # triennial vancouver region
  82-87, #Jig 4B
  13, # inside longline?
  15, # SOG lingcod
  39, 40, 48, 93, 76, 92)
spp <- "North Pacific Spiny Dogfish"

## Outside

maj <- get_major_areas()
ssid <- c(0,
          #1,
          2,3,
          #4,
          5,
          #6,7, 8, 9, 10, 11,
          14,
          22,
          # 25, # thornyhead
          21, 26,
          35, 36,
          # 46, 48,
          49, 50, 51,
          67, 68,
          81, 94)

ssid <- c(
  #OUTSIDE
          1,
          2,3,
          4,
          5,
          6,7, 8, 9, 10, 11,
          14,
          22,
          25, # thornyhead
          21, 26,
          35, 36,
          46, 48,
          49, 50, 51,
          67, 68,
          81, 94,
          #INSIDE
          45, #SOG trawl
20, # synoptic vancouver region?
79, # triennial vancouver region
82-87, #Jig 4B
13, # inside longline?
15, # SOG lingcod
39, 40, 48, 93, 76, 92
)



spp <- "North Pacific Spiny Dogfish"
dset <- compare_survey_sets(spp = spp, ssid = c(35, 41, 42, 43))

dset <- compare_survey_sets(spp = spp, ssid = c(6,7))


spp <- c( # "Lingcod",
         "Yelloweye Rockfish")

major_areas <- c("0","00","01", "02", "03", "04", "05", "06", "07", "08", "09", "11")


dsam <- compare_survey_samples(spp = spp, ssid = ssid, areas = major_areas)

dsam <- compare_survey_samples(spp = spp, ssid = c(6,7))

dsam <- compare_survey_samples(spp = spp, ssid = ssid)

saveRDS(dset, "test-yelloweye-outside-sets.rds")
saveRDS(dsam, "test-yelloweye-outside-samples-all.rds")

x <- dsam$x

check <- x |> filter(fn == 2)


xx <- x |> group_by(fn, ssid, species, survey_series_desc) |>
  summarise(n = n())

dd <- x[duplicated(x$specimen_id),]
dd1 <- filter(x, (specimen_id %in% c(dd$specimen_id)))


xx3 <- x |>
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

# Summary of returns
s <- dset$s
