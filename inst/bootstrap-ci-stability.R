library(data.table)
library(ggplot2)

# Source bootstrap functions
source("inst/bootstrap-clean.R")

# Reset to original SYN QCS survey
d <- readRDS("~/src/gfsynopsis-2024/report/data-cache-2025-03/pacific-cod.rds")$survey_sets
qcs <- subset(d, survey_abbrev == "SYN QCS")

# Function to test CI stability across increments
test_ci_stability <- function(dat, max_reps = 5000, increment = 100) {
  increments <- seq(increment, max_reps, by = increment)

  cat("Testing CI stability from", increment, "to", max_reps, "bootstrap samples\n")
  cat("Increments:", length(increments), "\n\n")

  # Run full bootstrap once and subsample
  set.seed(123)
  full_results <- boot_all_years_parallel_dt(dat, reps = max_reps)

  # For each year, we need to re-run boot with different R values to get proper CIs
  stability_results <- vector("list", length(increments))

  for (i in seq_along(increments)) {
    n_reps <- increments[i]
    cat("Testing", n_reps, "bootstrap samples...\n")

    # Run bootstrap for this increment
    set.seed(123)  # Same seed for consistency
    results <- boot_all_years_parallel_dt(dat, reps = n_reps)

    # Store results
    stability_results[[i]] <- data.table(
      n_bootstrap = n_reps,
      year = results$year,
      lwr = results$lwr,
      upr = results$upr,
      index = results$index,
      cv = results$cv
    )
  }

  # Combine all results
  combined <- rbindlist(stability_results)

  return(combined)
}

# Run stability test
stability_data <- test_ci_stability(qcs, max_reps = 5000, increment = 500)

# Plot CI bounds by bootstrap sample size for each year
ci_stability_plot <- ggplot(stability_data, aes(x = n_bootstrap)) +
  geom_line(aes(y = lwr, color = factor(year)), alpha = 0.7) +
  geom_line(aes(y = upr, color = factor(year)), alpha = 0.7) +
  labs(
    title = "Bootstrap CI Stability by Sample Size",
    subtitle = "How confidence intervals change with number of bootstrap samples",
    x = "Number of Bootstrap Samples",
    y = "Biomass",
    color = "Year"
  ) +
  theme_minimal() +
  facet_wrap(~year, scales = "free_y", ncol = 3)

print(ci_stability_plot)

# Plot CI width by bootstrap sample size
stability_data[, ci_width := upr - lwr]

ci_width_plot <- ggplot(stability_data, aes(x = n_bootstrap, y = ci_width, color = factor(year))) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Confidence Interval Width vs Bootstrap Samples",
    subtitle = "How CI width stabilizes with more bootstrap samples",
    x = "Number of Bootstrap Samples",
    y = "CI Width (Upper - Lower)",
    color = "Year"
  ) +
  theme_minimal()

print(ci_width_plot)

# Plot CV by bootstrap sample size
cv_stability_plot <- ggplot(stability_data, aes(x = n_bootstrap, y = cv, color = factor(year))) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Coefficient of Variation vs Bootstrap Samples",
    subtitle = "How CV stabilizes with more bootstrap samples",
    x = "Number of Bootstrap Samples",
    y = "Coefficient of Variation",
    color = "Year"
  ) +
  theme_minimal()

print(cv_stability_plot)

# Summary statistics
cat("\nCI Width Stability Summary:\n")
cat("===========================\n")
width_summary <- stability_data[, .(
  mean_ci_width = mean(ci_width),
  sd_ci_width = sd(ci_width),
  cv_ci_width = sd(ci_width) / mean(ci_width)
), by = n_bootstrap]

print(width_summary)

# Calculate percentage change in mean CI width
width_summary[, pct_change := c(NA, diff(mean_ci_width) / mean_ci_width[-length(mean_ci_width)] * 100)]

cat("\nPercentage change in mean CI width:\n")
print(width_summary[!is.na(pct_change), .(n_bootstrap, pct_change)])

# Find stabilization point (< 2% change)
stable_point <- width_summary[abs(pct_change) < 2 & !is.na(pct_change), min(n_bootstrap)]
if (length(stable_point) > 0) {
  cat(sprintf("\nCI width appears stable at: %d bootstrap samples\n", stable_point))
  cat("(< 2% change from previous increment)\n")
} else {
  cat("\nConsider testing with more bootstrap samples for stability.\n")
}
