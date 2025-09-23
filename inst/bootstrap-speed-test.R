library(dplyr)
library(tictoc)

# Source the bootstrap functions
source("inst/bootstrap-example.R")

# Prepare data
# qcs <- gfdata::get_survey_sets("pacific cod", ssid = c(1))
d <- readRDS("~/src/gfsynopsis-2024/report/data-cache-2025-03/pacific-cod.rds")$survey_sets
qcs <- subset(d, survey_abbrev == "SYN QCS")

# Test parameters
reps <- 2000

cat("Bootstrap Speed Test Comparison\n")
cat("================================\n")
cat("Bootstrap samples:", reps, "\n")
cat("Number of cores available:", parallel::detectCores(), "\n\n")

# 1. Sequential (original)
cat("1. Sequential bootstrapping...\n")
tic("Sequential")
result_sequential <- boot_all_years(qcs, reps = reps)
time_sequential <- toc(quiet = TRUE)

# 2. Parallel by year
cat("2. Parallel by year (furrr)...\n")
future::plan(future::multisession)
tic("Parallel by year")
result_parallel_year <- boot_all_years_parallel(qcs, reps = reps)
time_parallel_year <- toc(quiet = TRUE)

# 3. Parallel by boot (within each year)
cat("3. Parallel by boot (within boot::boot)...\n")
tic("Parallel by boot")
result_parallel_boot <- qcs %>%
  split(qcs$year) %>%
  purrr::map_dfr(boot_one_year_parallel, reps = reps, .id = "year")
result_parallel_boot$year <- as.numeric(result_parallel_boot$year)
time_parallel_boot <- toc(quiet = TRUE)

# 4. data.table sequential
cat("4. data.table sequential...\n")
tic("data.table sequential")
result_dt_sequential <- boot_all_years_dt(qcs, reps = reps)
time_dt_sequential <- toc(quiet = TRUE)

# 5. data.table parallel
cat("5. data.table parallel by boot...\n")
tic("data.table parallel")
result_dt_parallel <- boot_all_years_parallel_dt(qcs, reps = reps)
time_dt_parallel <- toc(quiet = TRUE)

# Results summary
cat("\nSpeed Test Results:\n")
cat("===================\n")
cat(sprintf("Sequential (dplyr):         %.2f seconds\n", time_sequential$toc - time_sequential$tic))
cat(sprintf("Parallel by year (dplyr):   %.2f seconds\n", time_parallel_year$toc - time_parallel_year$tic))
cat(sprintf("Parallel by boot (dplyr):   %.2f seconds\n", time_parallel_boot$toc - time_parallel_boot$tic))
cat(sprintf("Sequential (data.table):    %.2f seconds\n", time_dt_sequential$toc - time_dt_sequential$tic))
cat(sprintf("Parallel by boot (data.table): %.2f seconds\n", time_dt_parallel$toc - time_dt_parallel$tic))

# Speedup calculations
sequential_time <- time_sequential$toc - time_sequential$tic
parallel_year_time <- time_parallel_year$toc - time_parallel_year$tic
parallel_boot_time <- time_parallel_boot$toc - time_parallel_boot$tic
dt_sequential_time <- time_dt_sequential$toc - time_dt_sequential$tic
dt_parallel_time <- time_dt_parallel$toc - time_dt_parallel$tic

cat("\nSpeedup vs Sequential (dplyr):\n")
cat("==============================\n")
cat(sprintf("Parallel by year:           %.2fx faster\n", sequential_time / parallel_year_time))
cat(sprintf("Parallel by boot:           %.2fx faster\n", sequential_time / parallel_boot_time))
cat(sprintf("Sequential data.table:      %.2fx faster\n", sequential_time / dt_sequential_time))
cat(sprintf("Parallel data.table:        %.2fx faster\n", sequential_time / dt_parallel_time))

# Clean up
future::plan(future::sequential)

cat("\nTest completed!\n")
