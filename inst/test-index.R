devtools::load_all()

d <- boot_all_years_parallel_dt(dat, reps = 0, ncpus = 1)
head(d)
dplyr::glimpse(d)
