x <- readRDS("/Volumes/Extreme-SSD/src/gfsynopsis-2021/report/data-cache/north-pacific-spiny-dogfish.rds")

sets <- x$survey_sets
samps <- x$survey_samples

library(tidyverse)

sets <- filter(sets, survey_abbrev == "SYN QCS")
samps <- filter(samps, survey_abbrev == "SYN QCS")

glimpse(samps)
glimpse(sets)

y <- left_join(sets, select(samps, year, survey_abbrev, survey_id, species_code, sample_id, length, weight, specimen_id))

plot(sets$catch_weight, sets$catch_count)

filter(sets, catch_count > 10) %>% glimpse()

filter(y, sample_id == 233645) %>% glimpse

filter(y, is.na(sample_id)) %>% glimpse

group_by(y, sample_id) %>%
  summarise(n = length(unique(specimen_id)))

filter(y, !is.na(weight))
filter(y, is.na(weight))

filter(samps, !is.na(weight))
filter(samps, is.na(weight))

x <- filter(y, !is.na(sample_id)) %>% tail %>% glimpse

x <- filter(y, fishing_event_id == 5491292)

# need a function with logic for an individual fishing event:
get_nfish <- function(x, too_small = 0.1, too_big = 12) {

  stopifnot(length(unique(x$catch_count)) == 1L)
  stopifnot(length(unique(x$catch_weight)) == 1L)

  tot_samp_weight <- sum(x$weight, na.rm = TRUE) / 1000
  catch_weight <- x$catch_weight[1]

  if (is.na(tot_samp_weight) || tot_samp_weight == 0) { # no samples, check if count looks reasonable:
    implied_weight_per_fish <- catch_weight / x$catch_count
    if (implied_weight_per_fish < too_small || implied_weight_per_fish > too_big)
      stop("Implied fish weight too small or big.", call. = FALSE)
    n_fish_samp <- x$catch_count[1]
  } else {
    ratio <- tot_samp_weight / catch_weight
    if (ratio < 0.95 || ratio > 1.05)
      stop("total sample weight / catch weight is beyond the stated tolerance.", call. = FALSE)
    n_fish_samp <- nrow(x)
  }
  n_fish_samp
}

x <- filter(y, fishing_event_id == 5491292)
get_nfish(x)

x <- filter(y, fishing_event_id == 4363101)
get_nfish(x)



