
library(tidyverse)
devtools::load_all(".")

source(here::here("compare", "R", "functions-sets.R"))
source(here::here("compare", "R", "functions-samples.R"))

spp <- "pink shrimp (smooth)"
ds <- compare_survey_sets(spp = spp, ssid = c(2))
xs2 <- ds$x |> group_by(fn, ssid, species, survey_abbrev, major_stat_area_code) |>
  summarise(n = n())
# either function works, original misses only 2 sets both 0s due to grouping_code differences

dd <- ds$x[duplicated(ds$x$fishing_event_id),]
# no duplicates

ssid <- c(48)
spp <- "North Pacific Spiny Dogfish"
dset <- compare_survey_sets(spp = spp, ssid = ssid)
dsam <- compare_survey_samples(spp = spp, ssid = ssid)

x2 <- dset$x

xx <- dset$x |> group_by(fn, ssid, species, survey_abbrev) |>
  summarise(n = n())


xx3 <- d3$x |>
  group_by(fn, ssid, species, survey_series_id, major_stat_area_code, year, usability_desc) |>
  summarise(n = n())

xx4 <- d_feg$x |>
  group_by(fn, ssid, species, survey_abbrev, major_stat_area_code, year, usability_desc) |>
  summarise(n = n())

# Unlike values
u <- d$u

# Summary of returns
s <- d$s
