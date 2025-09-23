#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table uniqueN
#' @importFrom data.table rbindlist
#' @importFrom data.table .N

# Calculate design-based biomass estimate using data.table
calc_bio_dt <- function(dat, i = seq_len(nrow(dat))) {
  dt <- as.data.table(dat[i, ])
  dt_calc <- dt[, .(
    density = mean(density_kgpm2 * 1e6),
    grouping_area_km2 = unique(grouping_area_km2)
  ), by = .(year, survey_id, grouping_code)]
  dt_calc[, biomass := sum(density * grouping_area_km2), by = year]
  dt_calc[, unique(biomass)]
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
    biomass = calc_bio_dt(x),
    lowerci = bci$percent[[4]],
    upperci = bci$percent[[5]],
    re = stats::sd(b$t) / mean(b$t),
    num_sets = nrow(x), # we have one year of data here
    num_pos_sets = nrow(x[x$density_kgpm2 > 0,,drop=FALSE]),
    survey_abbrev = x$survey_abbrev[1],
    survey_series_desc = x$survey_series_desc[1],
    species_common_name = x$species_common_name[1],
    species_science_name = x$species_science_name[1]
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

get_design_index <- function(species, ssid = NULL, reps = 1000) {

  message("Retreiving survey set data")
  if (length(species) > 1L) {
    stop("get_design_index() only works with individual species.",   call. = FALSE)
  }
  if (length(ssid) > 1L) {
    stop("get_design_index() only works with individual surveys.",   call. = FALSE)
  }

  dat <- get_all_survey_sets(
    species = species,
    ssid = ssid,
    grouping_only = TRUE,
    remove_false_zeros = FALSE,
    usability = c(0, 1, 2, 6)
  )

  # longline:
  if (ssid %in% c(
    14, # IPHC FISS
    22, # HBLL OUT N
    36, # HBLL OUT S
    39, # HBLL INS N
    40, # HBLL INS S
    41, # SABLE INLET
    43, # SABLE RAND
    76  # OTHER DOG
  )) {
    message("Detected a longline or trap survey; using counts rather than weight")
    dat$density_kgpm2 <- dat$density_ppkm2 / 1e6
  }

  message("Calculating design-based index with bootstrapped uncertainty")
  ind <- boot_all_years_parallel_dt(dat, reps = reps)
  ind <- as.data.frame(ind)
  add_version(as_tibble(ind))
}
