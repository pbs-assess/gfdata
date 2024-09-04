### Get all Yelloweye Rockfish samples from database

remotes::install_github("pbs-assess/gfdata", ref = "trials")
library(tidyverse)
library(gfdata)

### Species --------------------------------------------------------------------
spp <- "Yelloweye Rockfish"

### Surveys --------------------------------------------------------------------
ssid <- NULL # will return everything

### MAJOR AREAS ----------------------------------------------------------------
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99")
## all canadian waters
## filter to exclude "01" if the Strait of Georgia not wanted

### GET SAMPLES --------------------------------------------------------------------

d <- get_all_survey_samples(species = spp,
                             ssid = ssid,
                             major = major_areas,
                             # include_event_info = TRUE #*
                             remove_duplicates = TRUE)

# * adds variables like date, depth, gear, etc., but causes strange duplication
# of ~ 18 YE samples from QCS synoptic due to missing boot_defaults

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


saveRDS(d, "yelloweye-samples-all-0903b.rds")

write.csv(dx, "yelloweye-sample-summary-0903b.csv")

