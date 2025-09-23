library(data.table)
library(ggplot2)

# Load data
d <- readRDS("~/src/gfsynopsis-2024/report/data-cache-2025-03/pacific-cod.rds")$survey_sets
survey <- "SYN HS"
survey <- "HBLL OUT N"
qcs <- subset(d, survey_abbrev == survey)
dplyr::glimpse(qcs)

if (survey == "HBLL OUT N") {
  qcs$density_kgpm2 <- qcs$density_ppkm2 / 1e6
}

# Calculate design-based biomass estimate using data.table
calc_bio_dt <- function(dat, i = seq_len(nrow(dat))) {
  dt <- as.data.table(dat[i, ])
  dt[, density := mean(density_kgpm2 * 1e6), by = .(year, survey_id, area_km2, grouping_code)]
  dt[, biomass := sum(unique(density) * unique(area_km2)), by = year]
  dt[, unique(biomass)]
}

# Bootstrap one year with parallel processing
boot_one_year_parallel_dt <- function(x, reps, ncpus = NULL) {
  if (is.null(ncpus)) {
    ncpus <- parallel::detectCores()
  }
  b <- boot::boot(x, statistic = calc_bio_dt, strata = x$grouping_code, R = reps,
                  parallel = "multicore", ncpus = ncpus)
  suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
  data.table(
    index = mean(b$t),
    median_boot = median(b$t),
    lwr = bci$percent[[4]],
    upr = bci$percent[[5]],
    cv = sd(b$t) / mean(b$t),
    biomass = calc_bio_dt(x)
  )
}

# Bootstrap all years with parallel processing
boot_all_years_parallel_dt <- function(dat, reps, ncpus = NULL) {
  dat_list <- split(dat, dat$year)
  result_list <- lapply(dat_list, boot_one_year_parallel_dt, reps = reps, ncpus = ncpus)
  out <- rbindlist(result_list, idcol = "year")
  out[, year := as.numeric(year)]
  out
}

# Run bootstrap analysis
cat("Running parallel bootstrap with data.table...\n")
cat("Cores available:", parallel::detectCores(), "\n")
cat("Using cores:", parallel::detectCores(), "\n\n")

system.time({
  results <- boot_all_years_parallel_dt(qcs, reps = 1000)
})

print(results)

dind <- readRDS("~/src/gfsynopsis-2024/report/data-cache-2025-03/pacific-cod.rds")$survey_index
d_index <- subset(dind, survey_abbrev == survey)

# Compare bootstrap results with original index
comparison_plot <- ggplot() +
  geom_line(data = results, aes(x = year, y = biomass), color = "blue", size = 1) +
  geom_ribbon(data = results, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.3, fill = "blue") +
  geom_line(data = d_index, aes(x = year, y = biomass), color = "red", size = 1) +
  geom_ribbon(data = d_index, aes(x = year, ymin = lowerci, ymax = upperci), alpha = 0.3, fill = "red") +
  labs(
    title = "Bootstrap vs Original Survey Index",
    subtitle = "Blue = Bootstrap estimates, Red = Original estimates",
    x = "Year",
    y = "Biomass"
  ) +
  theme_minimal()

print(comparison_plot)

results$biomass - d_index$biomass
