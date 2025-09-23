library(data.table)
library(ggplot2)

# Source bootstrap functions
source("inst/bootstrap-clean.R")

# Reset to original SYN QCS survey
d <- readRDS("~/src/gfsynopsis-2024/report/data-cache-2025-03/pacific-cod.rds")$survey_sets
qcs <- subset(d, survey_abbrev == "SYN QCS")

# Function to run bootstrap multiple times and calculate standard error
test_convergence <- function(dat, reps, n_runs = 10) {
  cat("Testing", reps, "bootstrap samples with", n_runs, "runs...\n")

  results_list <- vector("list", n_runs)

  for (i in 1:n_runs) {
    set.seed(123 + i)  # Different seed for each run
    results_list[[i]] <- boot_all_years_parallel_dt(dat, reps = reps)
  }

  # Combine all results
  combined <- rbindlist(results_list, idcol = "run")

  # Calculate standard error across runs for each year using bootstrap CI bounds
  se_by_year <- combined[, .(
    mean_lwr = mean(lwr),
    se_lwr = sd(lwr),
    cv_lwr = sd(lwr) / mean(lwr),
    mean_upr = mean(upr),
    se_upr = sd(upr),
    cv_upr = sd(upr) / mean(upr),
    mean_index = mean(index),
    se_index = sd(index),
    cv_index = sd(index) / mean(index)
  ), by = year]

  # Overall SE across all years using bootstrap index
  overall_se <- combined[, .(
    overall_se_index = sd(index),
    overall_cv_index = sd(index) / mean(index),
    overall_se_lwr = sd(lwr),
    overall_cv_lwr = sd(lwr) / mean(lwr),
    overall_se_upr = sd(upr),
    overall_cv_upr = sd(upr) / mean(upr)
  )]

  list(
    n_reps = reps,
    se_by_year = se_by_year,
    overall_se_index = overall_se$overall_se_index,
    overall_cv_index = overall_se$overall_cv_index,
    overall_se_lwr = overall_se$overall_se_lwr,
    overall_cv_lwr = overall_se$overall_cv_lwr,
    overall_se_upr = overall_se$overall_se_upr,
    overall_cv_upr = overall_se$overall_cv_upr
  )
}

# Test different numbers of bootstrap samples
bootstrap_sizes <- c(100, 250, 500, 1000, 2000, 4000)
convergence_results <- vector("list", length(bootstrap_sizes))

cat("Testing bootstrap convergence...\n")
cat("================================\n")

for (i in seq_along(bootstrap_sizes)) {
  convergence_results[[i]] <- test_convergence(qcs, bootstrap_sizes[i], n_runs = 5)
}

# Extract overall results for bootstrap index (mean of bootstrap samples)
convergence_summary <- data.table(
  n_bootstrap = sapply(convergence_results, function(x) x$n_reps),
  overall_se_index = sapply(convergence_results, function(x) x$overall_se_index),
  overall_cv_index = sapply(convergence_results, function(x) x$overall_cv_index),
  overall_se_lwr = sapply(convergence_results, function(x) x$overall_se_lwr),
  overall_cv_lwr = sapply(convergence_results, function(x) x$overall_cv_lwr),
  overall_se_upr = sapply(convergence_results, function(x) x$overall_se_upr),
  overall_cv_upr = sapply(convergence_results, function(x) x$overall_cv_upr)
)

print(convergence_summary)

# Plot convergence for bootstrap index
convergence_plot <- ggplot(convergence_summary, aes(x = n_bootstrap)) +
  geom_line(aes(y = overall_se_index), color = "blue", size = 1) +
  geom_point(aes(y = overall_se_index), color = "blue", size = 2) +
  labs(
    title = "Bootstrap Convergence Analysis - Bootstrap Index",
    subtitle = "Standard Error of Bootstrap Index Across Multiple Runs",
    x = "Number of Bootstrap Samples",
    y = "Standard Error of Bootstrap Index"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = bootstrap_sizes)

print(convergence_plot)

# CV plot for bootstrap index
cv_plot <- ggplot(convergence_summary, aes(x = n_bootstrap)) +
  geom_line(aes(y = overall_cv_index), color = "red", size = 1) +
  geom_point(aes(y = overall_cv_index), color = "red", size = 2) +
  labs(
    title = "Bootstrap Coefficient of Variation - Bootstrap Index",
    subtitle = "CV of Bootstrap Index Across Multiple Runs",
    x = "Number of Bootstrap Samples",
    y = "Coefficient of Variation"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = bootstrap_sizes)

print(cv_plot)

# Plot convergence for confidence intervals
ci_plot <- ggplot(convergence_summary, aes(x = n_bootstrap)) +
  geom_line(aes(y = overall_se_lwr), color = "green", size = 1) +
  geom_point(aes(y = overall_se_lwr), color = "green", size = 2) +
  geom_line(aes(y = overall_se_upr), color = "orange", size = 1) +
  geom_point(aes(y = overall_se_upr), color = "orange", size = 2) +
  labs(
    title = "Bootstrap Convergence Analysis - Confidence Intervals",
    subtitle = "Green = Lower CI, Orange = Upper CI",
    x = "Number of Bootstrap Samples",
    y = "Standard Error of CI Bounds"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = bootstrap_sizes)

print(ci_plot)

cat("\nRecommendation:\n")
cat("================\n")
improvement_index <- diff(convergence_summary$overall_se_index) / convergence_summary$overall_se_index[-length(convergence_summary$overall_se_index)]
cat("SE improvement for bootstrap index from increasing samples:\n")
for (i in 2:nrow(convergence_summary)) {
  pct_improvement <- abs(improvement_index[i-1]) * 100
  cat(sprintf("%d -> %d samples: %.1f%% improvement\n",
              convergence_summary$n_bootstrap[i-1],
              convergence_summary$n_bootstrap[i],
              pct_improvement))
}

# Find point of diminishing returns (< 5% improvement) for bootstrap index
threshold_idx <- which(abs(improvement_index) < 0.05)[1]
if (!is.na(threshold_idx)) {
  recommended_n <- convergence_summary$n_bootstrap[threshold_idx]
  cat(sprintf("\nRecommended minimum: %d bootstrap samples\n", recommended_n))
  cat("(Point where further increases yield < 5% improvement in bootstrap index SE)\n")
} else {
  cat("\nConsider testing with more bootstrap samples.\n")
}
