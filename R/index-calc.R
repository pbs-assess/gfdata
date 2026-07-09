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
    density = mean(density_kgpm2 * 1e6, na.rm = TRUE),
    grouping_area_km2 = unique(grouping_area_km2)
  ), by = .(year, survey_id, grouping_code)]
  dt_calc[, biomass := sum(density * grouping_area_km2), by = year]
  dt_calc[, unique(biomass)]
}

# Vectorized stratified bootstrap of total biomass for a single year.
#
# boot::boot() with `strata` resamples each stratum (grouping_code) with
# replacement to its original size, so the *set* of groups contributing to
# each replicate never changes -- only which rows within a group get drawn.
# That means the whole bootstrap distribution can be generated with one
# sample.int()/colSums() pass per group instead of calling a per-replicate
# statistic function (previously calc_bio_dt() via data.table) reps times,
# which is what made the old boot::boot() + multicore-fork approach slow for
# small per-year data sets.
boot_biomass_vec <- function(x, reps) {
  density <- x$density_kgpm2 * 1e6
  not_na <- !is.na(density)
  density[!not_na] <- 0
  grouping_code <- x$grouping_code
  area <- x$grouping_area_km2

  biomass_boot <- numeric(reps)
  for (g in unique(grouping_code)) {
    i <- which(grouping_code == g)
    ng <- length(i)
    samp <- sample.int(ng, size = ng * reps, replace = TRUE)
    val_sum <- .colSums(matrix(density[i][samp], ng, reps), ng, reps)
    n_valid <- .colSums(matrix(not_na[i][samp], ng, reps), ng, reps)
    biomass_boot <- biomass_boot + (val_sum / n_valid) * area[i][1]
  }
  biomass_boot
}

# Bootstrap one year
boot_one_year_dt <- function(x, reps) {
  do_boot <- reps > 0

  # A stratum with zero non-missing density values (e.g. all sets missing
  # swept-area/effort data needed to compute density_kgpm2) silently produces
  # NaN biomass/variance downstream, surfacing many calls later as a cryptic
  # boot::boot.ci() error. Fail loudly here instead, naming the year and
  # stratum so the underlying data problem is easy to find.
  na_by_group <- tapply(x$density_kgpm2, x$grouping_code, function(d) all(is.na(d)))
  bad_groups <- names(na_by_group)[na_by_group]
  if (length(bad_groups) > 0) {
    stop(
      "In year ", x$year[1], ", grouping_code(s) ", paste(bad_groups, collapse = ", "),
      " have no non-missing density_kgpm2 values (all sets NA). ",
      "This usually indicates missing effort/swept-area data upstream rather than ",
      "a genuine absence of sets; check the underlying survey set data.",
      call. = FALSE
    )
  }

  b_analytical <- calc_bio_dt(x)

  if (do_boot) {
    t <- boot_biomass_vec(x, reps)
    # boot::boot.ci() silently returns NULL (just prints a message, no
    # warning/error) when every bootstrap replicate is identical -- common
    # for years with very few sets per stratum, where resampling can't
    # produce any variation. bci$percent[[4]] on a NULL bci is also silently
    # NULL rather than NA, which drops the lowerci/upperci columns entirely
    # further down and breaks rbindlist() across years, so guard explicitly.
    suppressWarnings(bci <- boot::boot.ci(
      list(R = reps), type = "perc", t0 = b_analytical, t = t
    ))
    if (is.null(bci)) {
      do_boot <- FALSE
    }
  }

  x$density_scaled <- x$density_kgpm2 * 1e6

  # Some strata contain only one set (lonely PSU) in some years, which
  # survey::svydesign() rejects by default. Treat lonely PSUs as certainty
  # units: they still contribute to the point estimate but contribute zero
  # to that stratum's variance, rather than erroring out.
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "certainty")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)

  mydesign <- survey::svydesign(
    id = ~ 1, strata = ~ grouping_code,
    data = x, fpc = ~ grouping_area_km2
  )
  design_estimates <- survey::svytotal(~ density_scaled, design = mydesign)
  var_design <- as.numeric(attr(design_estimates, "var"))

  data.table(
    biomass = b_analytical,
    lowerci = if (do_boot) bci$percent[[4]] else NA_real_,
    upperci = if (do_boot) bci$percent[[5]] else NA_real_,
    re = if (do_boot) stats::sd(t) / mean(t) else NA_real_,
    cv_boot = if (do_boot) stats::sd(t) / mean(t) else NA_real_,
    variance_boot = if (do_boot) as.numeric(stats::var(t)) else NA_real_,
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

# Bootstrap all years
boot_all_years_dt <- function(dat, reps) {
  dat_list <- split(dat, dat$year)
  result_list <- lapply(dat_list, boot_one_year_dt, reps = reps)
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
    remove_false_zeros = FALSE,
    usability = c(0, 1, 2, 6)
  )
  } else {
    dat <- data[!is.na(data$grouping_code),,drop=FALSE] # grouping_only = TRUE
    dat <- dat[dat$usability_code %in% c(0, 1, 2, 6),,drop=FALSE]
  }

  if (!is.null(ssid)) {
    dat <- dat[dat$survey_series_id %in% ssid,,drop=FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No survey set data remain for species '", species, "'",
      if (!is.null(ssid)) paste0(" and ssid ", paste(ssid, collapse = ", ")) else "",
      " after filtering to usable grouping codes (usability_code %in% c(0, 1, 2, 6)) ",
      "and a non-missing grouping_code. Check that `data` contains rows for this ",
      "species/ssid combination and that `usability_code`/`grouping_code` are populated.",
      call. = FALSE
    )
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
  ind <- boot_all_years_dt(dat, reps = reps)
  ind <- as.data.frame(ind)
  add_version(as_tibble(ind))
}
