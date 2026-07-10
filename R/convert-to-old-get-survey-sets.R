convert_to_old_sets <- function(x) {

  make_na_like <- function(type, n) {
    if (type == "POSIXct") {
      return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC"))
    }
    if (type == "integer") {
      return(rep(NA_integer_, n))
    }
    if (type == "numeric") {
      return(rep(NA_real_, n))
    }
    if (type == "character") {
      return(rep(NA_character_, n))
    }
    rep(NA, n)
  }

  coerce_like <- function(x, type) {
    if (type == "POSIXct") {
      return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }
    if (type == "integer") {
      return(as.integer(x))
    }
    if (type == "numeric") {
      return(as.numeric(x))
    }
    if (type == "character") {
      return(as.character(x))
    }
    x
  }

  target_types <- c(
    survey_series_id = "numeric",
    survey_id = "numeric",
    species_code = "character",
    trip_id = "numeric",
    year = "integer",
    fishing_event_id = "numeric",
    fe_major_level_id = "numeric",
    latitude = "numeric",
    longitude = "numeric",
    grouping_code = "numeric",
    major_stat_area_code = "character",
    minor_stat_area_code = "character",
    depth_m = "numeric",
    duration_min = "integer",
    doorspread_m = "numeric",
    speed_mpm = "numeric",
    tow_length_m = "numeric",
    catch_weight = "numeric",
    density_kgpm2 = "numeric",
    catch_count = "numeric",
    density_pcpm2 = "numeric",
    skate_count = "integer",
    hook_count = "integer",
    density_ppkm2 = "numeric",
    survey_series_desc = "character",
    survey_abbrev = "character",
    month = "integer",
    day = "integer",
    time_deployed = "POSIXct",
    time_retrieved = "POSIXct",
    latitude_end = "numeric",
    longitude_end = "numeric",
    species_common_name = "character",
    species_science_name = "character",
    species_desc = "character",
    sample_id = "numeric",
    area_km2 = "integer"
  )
  target_names <- names(target_types)

  if (!"survey_series_id" %in% names(x) && "survey_series_id.x" %in% names(x)) {
    x$survey_series_id <- x$survey_series_id.x
  }

  aliases <- c(
    hook_count = "lglsp_hook_count",
    area_km2 = "grouping_area_km2"
  )

  for (nm in names(aliases)) {
    from <- aliases[[nm]]
    if (!nm %in% names(x) && from %in% names(x)) {
      x[[nm]] <- x[[from]]
    }
  }

  for (nm in target_names) {
    if (!nm %in% names(x)) {
      x[[nm]] <- make_na_like(target_types[[nm]], nrow(x))
    }
  }

  x <- x[target_names]

  for (nm in target_names) {
    x[[nm]] <- coerce_like(x[[nm]], target_types[[nm]])
  }

  x
}
