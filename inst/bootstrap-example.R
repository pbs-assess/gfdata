library(dplyr)

qcs <- gfdata::get_survey_sets("pacific cod", ssid = c(1))
# d <- readRDS("~/src/gfsynopsis-2021/report/data-cache/pacific-cod.rds")$survey_sets
# qcs <- subset(d, survey_abbrev == "SYN QCS")
names(qcs)

# calculate design-based biomass estimate from output of get_survey_sets()
calc_bio <- function(dat, i = seq_len(nrow(dat))) {
  dat[i, ] %>%
    group_by(year, survey_id, area_km2, grouping_code) %>%
    summarise(density = mean(density_kgpm2 * 1e6), .groups = "drop_last") %>%
    group_by(year) %>%
    summarise(biomass = sum(density * area_km2), .groups = "drop_last") %>%
    pull(biomass)
}

boot_one_year <- function(x, reps) {
  b <- boot::boot(x, statistic = calc_bio, strata = x$grouping_code, R = reps)
  suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
  tibble::tibble(
    index = mean(b$t),
    median_boot = median(b$t),
    lwr = bci$percent[[4]],
    upr = bci$percent[[5]],
    cv = sd(b$t) / mean(b$t),
    biomass = calc_bio(x)
  )
}

boot_all_years <- function(dat, reps) {
  out <- dat %>%
    split(dat$year) %>%
    purrr::map_dfr(boot_one_year, reps = reps, .id = "year")
  out$year <- as.numeric(out$year)
  out
}

boot_all_years_parallel <- function(dat, reps) {
  out <- dat %>%
    split(dat$year) %>%
    furrr::future_map_dfr(boot_one_year, reps = reps, .id = "year",
      .options = furrr::furrr_options(seed = TRUE))
  out$year <- as.numeric(out$year)
  out
}


ind <- boot_all_years(qcs, reps = 100) # need more than 100!

future::plan(future::multisession)
ind <- boot_all_years_parallel(qcs, reps = 100) # need more than 100!

head(ind)
