### Get all Lingcod samples from database

remotes::install_github("pbs-assess/gfdata", ref = "trials")
library(tidyverse)
library(gfdata)

### Species --------------------------------------------------------------------
spp <- "Lingcod"

### Surveys --------------------------------------------------------------------
ssid <- NULL # will return everything

### MAJOR AREAS ----------------------------------------------------------------
major_areas <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99")
## all canadian waters
## filter to exclude "01" if the Strait of Georgia not wanted

### GET SAMPLES --------------------------------------------------------------------

d <- get_all_survey_samples(species = spp,
                             ssid = ssid,
                             major = major_areas,
                             # include_event_info = TRUE #*
                             remove_duplicates = TRUE)

# * adds variables like date, depth, gear, etc.

dd <- d[duplicated(d$specimen_id),]

dx <- d |>
  group_by(survey_series_id, survey_series_desc) |>
  mutate(age_years = ifelse(!is.na(age), trip_year, NA)) |>
  summarise(n = n(),
            unique_sp = length(unique(specimen_id)),
            unique_fe = length(unique(fishing_event_id)),
            lengths = length(na.omit(length)),
            weights = length(na.omit(weight)),
            ages = length(na.omit(age)),
            maturities = sum(na.omit(maturity_code) != 9),
            min_age_year = min(age_years, na.rm = TRUE),
            max_age_year = max(age_years, na.rm = TRUE)
  )


saveRDS(d, "lingcod-samples-all-0827.rds")

write.csv(dx, "lingcod-sample-summary-0827.csv")

