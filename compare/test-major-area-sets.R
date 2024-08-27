### test major areas filter on survey sets
library(tidyverse)
library(gfdata)
# devtools::load_all(".")

### Species --------------------------------------------------------------------
spp <- "North Pacific Spiny Dogfish"
spp <- "Yelloweye Rockfish"
spp <- "Bluntnose Sixgill Shark"
### Surveys --------------------------------------------------------------------

ssid <- NULL
# ssid <- c(39, 40, 48, 93, 76, 92) # Inside dogfish
# ssid <- c(6,7) # MSSM
# ssid <- c(35) # SABLE
# ssid <- c(14) # IPHC

# ssid <- c(39) # HBLL 40, 76 all
# Error in if (any(fe$FE_SUB_LEVEL_ID > 1)) { :
# missing value where TRUE/FALSE needed


### MAJOR AREAS ----------------------------------------------------------------
major_areas <- NULL

## all canadian waters and unknown
major_areas <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99") # unknown "0","00",

# major_areas <- c("03","04") # WCVI
# major_areas <- c("01","68") # SOG and Puget Sound


### Sets -----------------------------------------------------------------------
d <- get_all_survey_sets(species = spp,
                          ssid = ssid,
                          major = major_areas,
                          include_activity_matches = FALSE,
                          remove_false_zeros = TRUE,
                          remove_duplicates = TRUE)


dd <- d[duplicated(d$fishing_event_id),]
x <- d |> mutate(duplicate = ifelse(fishing_event_id %in% c(unique(dd$fishing_event_id)) & (is.na(skate_count)|(skate_count == 1)), "Y", "N"))

# x <- filter(d, survey_series_id != 48)


dd1 <- filter(x, !(fishing_event_id %in% c(unique(dd$fishing_event_id))))
dd2 <- filter(x, (fishing_event_id %in% c(unique(dd$fishing_event_id))))

xx <- x  |>
  group_by(survey_series_id, survey_series_desc,
           duplicate,
           # original_ind,
           # reason_desc,
           # major_stat_area_code,
           # minor_stat_area_code
           ) |>
  summarise(n = n(), unique_fe = length(unique(fishing_event_id)))

saveRDS(d, "f2-sixgill-sets-all.rds")



### Samples --------------------------------------------------------------------

d2 <- get_all_survey_samples(species = spp,
                          ssid = ssid,
                          major = major_areas,
                          unsorted_only = FALSE, random_only = FALSE,
                          # include_event_info = FALSE, # TRUE causes strange duplication of ~ 18 YE samples from QCS synoptic due to missing boot_defaults
                          remove_duplicates = TRUE)
# d1 <- d2
x <- d2
dd <- x[duplicated(x$specimen_id),]
x <- x |> mutate(duplicate = ifelse(specimen_id %in% c(unique(dd$specimen_id)), "Y", "N"))

dd1 <- filter(x, !(specimen_id %in% c(dd$specimen_id)))
dd2 <- filter(x, (specimen_id %in% c(dd$specimen_id)))

xx2 <- x |> group_by(survey_series_id, survey_series_desc, duplicate,
                     # reason_desc,
                     # major_stat_area_code,
                     # minor_stat_area_code,
                     original_ind) |>
  summarise(n = n(),
            unique_sp = length(unique(specimen_id)),
            unique_fe = length(unique(fishing_event_id)))


saveRDS(d2, "f2-yelloweye-samples-all-major-for-39-ssids.rds")


xa <- x |> filter(!is.na(age))


xxa <- xa |>
  group_by(survey_series_id, survey_series_desc, duplicate,
                     # reason_desc,
                     major_stat_area_code,
                     # minor_stat_area_code,
                     original_ind) |>
  summarise(n = n(), unique_fe = length(unique(fishing_event_id)),
            min_year = min(year),
            max_year = max(year))
