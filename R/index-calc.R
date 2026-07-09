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
  do_boot <- reps > 0
  if (do_boot) {
    if (is.null(ncpus)) {
      ncpus <- parallel::detectCores()
    }
    b <- boot::boot(x, statistic = calc_bio_dt, strata = x$grouping_code, R = reps,
      parallel = "multicore", ncpus = ncpus)
    suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
  }

  x$density_scaled <- x$density_kgpm2 * 1e6
  mydesign <- survey::svydesign(
    id = ~ 1, strata = ~ grouping_code,
    data = x, fpc = ~ grouping_area_km2
  )
  design_estimates <- survey::svytotal(~ density_scaled, design = mydesign)
  var_design <- as.numeric(attr(design_estimates, "var"))

  b_analytical <- calc_bio_dt(x)

  data.table(
    biomass = b_analytical,
    lowerci = if (do_boot) bci$percent[[4]] else NA_real_,
    upperci = if (do_boot) bci$percent[[5]] else NA_real_,
    re = if (do_boot) stats::sd(b$t) / mean(b$t) else NA_real_,
    cv_boot = if (do_boot) stats::sd(b$t) / mean(b$t) else NA_real_,
    variance_boot = if (do_boot) as.numeric(stats::var(b$t)) else NA_real_,
    cv_design = sqrt(var_design) / b_analytical,
    variance_design = var_design,
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

#' @title Calculate a design index
#'
#' @description
#' Calculate a design-based index with bootstrapped standard errors
#'
#' @param species Species common name or number
#' @param ssid Survey series ID
#' @param reps Number of bootstrap samples. Set to `0` to skip the bootstrapping
#'   and only report design-based variance estimates.
#' @param data Optional output from a call to [get_all_survey_sets()].
#'   By default, this function will call [get_all_survey_sets()] for the
#'   given species, but this requires access to the GFBio database, and
#'   will also be slower if you've already cached the data.
#' @export
get_design_index <- function(species, ssid = NULL, reps = 1000, data = NULL) {

  message("Retreiving survey set data")
  if (length(species) > 1L) {
    stop("get_design_index() only works with individual species.",   call. = FALSE)
  }
  if (length(ssid) > 1L) {
    stop("get_design_index() only works with individual surveys.",   call. = FALSE)
  }
  if (ssid %in% c(41, 43)) {
    stop("Function not set up for the sablefish surveys yet", call. = FALSE)
  }

  if (is.null(data)) {
  dat <- get_all_survey_sets(
    species = species,
    ssid = ssid,
    grouping_only = TRUE,
    remove_false_zeros = TRUE,
    usability = c(0, 1, 2, 6)
  )
  } else {
    dat <- data[!is.na(data$grouping_code),,drop=FALSE] # grouping_only = TRUE
    dat <- dat[dat$grouping_code %in% c(0, 1, 2, 6),,drop=FALSE]
  }

  #   if (is_sable) {
  #    mb <- tapply(dat$CPUE_PPT, list(dat$GROUPING_CODE), mean)
  #    mu[sa$GROUPING_CODE %in% names(mb)] <- mb
  #    res <- mean(mu)
  # } else if (is_ll) {
  #    mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
  #    mu[sa$GROUPING_CODE %in% names(mb)] <- mb
  #    res <- sum(mu * sa$AREA_KM2)
  # } else if (is_jig) {
  #    mb <- tapply(dat$CPUE_PPH, list(dat$GROUPING_CODE), mean)
  # mu[sa$GROUPING_CODE %in% names(mb)] <- mb
  # res <- mean(mu)
  # } else if (is_dog) {
  #    mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
  #    mu[sa$GROUPING_CODE %in% names(mb)] <- mb
  #    res <- mean(mu)
  # } else {
  #    mb <- tapply(dat$DENSITY_KGPM2 * 1000000, list(dat$GROUPING_CODE), mean)
  #    mu[sa$GROUPING_CODE %in% names(mb)] <- mb
  #    res <- sum(mu * sa$AREA_KM2)
  # }
  # longline:
  if (ssid %in% c(
    14, # IPHC FISS
    22, # HBLL OUT N
    36, # HBLL OUT S
    39, # HBLL INS N
    40, # HBLL INS S
    # 41, # SABLE INLET
    # 43, # SABLE RAND
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
