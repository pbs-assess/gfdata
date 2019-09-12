# Hake effort data for Ole
# 2019-09-12

dir.create("scratch", showWarnings = FALSE)
library(dplyr)
library(ggplot2)

f <- "scratch/hake-raw.rds"
if (!file.exists(f)) {
  d <- gfdata::get_cpue_historical_hake()
  saveRDS(d, file = f)
} else {
  d <- readRDS(f)
}

# For data after the mid-1980's, the GFFOS Merged Catch Table can be filtered by
# all midwater trawl gear as a general measure of hake effort. In earlier
# records, midwater trawl gear was not differentiated from bottom trawl gear.
#
# To include effort before midwater trawl gear is an option in the database,
# records can also be extracted by all those with >= 500 kg of hake (regardless
# of reported gear type), as well as by a 'hake fleet' of vessels catching more
# than 500 kg hake on more than 100 tows ever, and on 5 or more trips per year
# for more than 5 years ever.

# 1. extract all midwater trawl records
gear <- d %>% filter(gear %in% c("midwater trawl", "midwater trawl (unknown type)"))
nrow(gear)

# 2. extract all records with >500 kg catch
min_catch <- d %>% filter(species_code == '225', total > 500)
nrow(min_catch)

# 3. define fleet as all vessels which have caught > 500 kg hake on more than
# 100 tows ever, and on 5 or more trips per year for more than 5 years
#
# This followed the same approach as gfplot::tidy-cpue-index().
#
# Positive catch here refers to tows which caught >= 500 kg hake.

min_positive_fe <- 100
min_positive_trips <- 5
min_yrs_with_trips <- 5

# hake tows >= 500 kg:
catch <- d %>% filter(species_code == '225', total > 500,
  hours_fished > 0, !is.na(vessel), !vessel == '0') %>%
  select(year, vessel, trip_id, fishing_event_id,
    species_code, total, hours_fished)

# figure out which vessels should be considered part of the hake fleet
fleet <- catch %>%
  group_by(vessel) %>%
  mutate(total_positive_tows = sum(total > 500)) %>%
  filter(total_positive_tows >= min_positive_fe) %>%
  group_by(year, vessel) %>%
  summarise(n_trips_per_year = length(unique(trip_id))) %>%
  filter(n_trips_per_year >= min_positive_trips) %>%
  group_by(vessel) %>%
  summarise(n_years = length(unique(year))) %>%
  filter(n_years >= min_yrs_with_trips)

# retain the data from our "fleet"
fleet_catch <- semi_join(d, fleet, by = "vessel")
nrow(fleet_catch)

all_hake <- dplyr::bind_rows(gear, min_catch) %>%
  dplyr::bind_rows(fleet_catch)
all_hake <- dplyr::distinct(all_hake)
nrow(all_hake)

d <- all_hake %>%
  filter(year <= 2018)

d$area_grouped <- gfplot::assign_areas(d$major_stat_area_description,
  area_regex = c("3[C]+", "3[D]+", "5[ABC]+", "5[DE]+", "4[B]+"))
table(d$area_grouped)

month_lookup1 <- data.frame(month = 1:12,
  month_group1 = c(4, 4, 4, 1, 1, 2, 2, 3, 3, 3, 4, 4))

month_lookup2 <- data.frame(month = 1:12,
  month_group2 = c(4, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4))

d <- left_join(d, month_lookup1) %>%
  left_join(month_lookup2)

d2 <- d %>% group_by(trip_id) %>%
  mutate(frac_hake =
      sum(total[species_common_name == "PACIFIC HAKE"]) / sum(total)) %>%
  filter(frac_hake > 0.75)

get_days <- function(x) {
  out <- lubridate::time_length(
    lubridate::interval(min(x), max(x)), "day")
  out
}

# First month grouping -------------------------------------

d3 <- d2 %>%
  group_by(year, month_group1, area_grouped, trip_id) %>%
  summarize(n_fished_days = get_days(best_date),
    vessel = unique(vessel))
hist(d3$n_fished_days)
nrow(d3)
nrow(filter(d3, n_fished_days > 10))
nrow(filter(d3, n_fished_days > 25))
nrow(filter(d3, n_fished_days > 50))

d4 <- filter(d3, n_fished_days <= 30) %>%
  group_by(year, month_group1, area_grouped) %>%
  summarize(n_vessel = length(unique(vessel)),
    n_fished_days = sum(n_fished_days))

all <- d4 %>%
  mutate(type = ">= 0") %>%
  mutate(n_fished_days = n_fished_days + 8)

privacy <- d4 %>% filter(n_vessel >= 3) %>%
  mutate(type = ">= 3")

bind_rows(all, privacy) %>%
  ggplot(aes(year, n_fished_days, colour = type)) + geom_line() +
  facet_grid(month_group1~area_grouped)
ggsave("scratch/hake-2019-09-12-ole-1.pdf", width = 8, height = 6)

bind_rows(privacy) %>%
  ggplot(aes(year, n_fished_days, colour = type)) + geom_line() +
  facet_grid(month_group1~area_grouped)
ggsave("scratch/hake-2019-09-12-privacy-1.pdf", width = 8, height = 6)

privacy %>%
  select(-n_vessel, -type) %>%
  saveRDS(file = "scratch/hake-2019-09-12-ole-1.rds")

# Second month grouping -------------------------------------

d3_2 <- d2 %>%
  group_by(year, month_group2, area_grouped, trip_id) %>%
  summarize(n_fished_days = get_days(best_date),
    vessel = unique(vessel))

d4_2 <- filter(d3_2, n_fished_days <= 30) %>%
  group_by(year, month_group2, area_grouped) %>%
  summarize(n_vessel = length(unique(vessel)),
    n_fished_days = sum(n_fished_days))

all_2 <- d4_2 %>%
  mutate(type = ">= 0")

privacy_2 <- d4_2 %>% filter(n_vessel >= 3) %>%
  mutate(type = ">= 3")

bind_rows(all_2, privacy_2) %>%
  ggplot(aes(year, n_fished_days, colour = type)) + geom_line() +
  facet_grid(month_group2~area_grouped)
ggsave("scratch/hake-2019-09-12-ole-2.pdf", width = 8, height = 6)

bind_rows(privacy_2) %>%
  ggplot(aes(year, n_fished_days, colour = type)) + geom_line() +
  facet_grid(month_group2~area_grouped)
ggsave("scratch/hake-2019-09-12-privacy-2.pdf", width = 8, height = 6)

privacy_2 %>%
  select(-n_vessel, -type) %>%
  saveRDS(file = "scratch/hake-2019-09-12-ole-2.rds")
