# Compare survey sets
compare_survey_sets <- function (spp,
                                 ssids = NULL,
                                 drop_if_all_zero = FALSE,
                                 ...
                                 ) {

  # Store '...' args as list of named values
  arg_list <- list(...)
  # Prepare argument list for each function
  get_args <- arg_list[ names(arg_list) %in% names(formals(get_survey_sets)) ]
  get_all_args <- arg_list[ names(arg_list) %in% names(formals(get_all_survey_sets)) ]

  if(is.null(ssids)) {
    ssid <- c(1, 3, 4, 16, 2, 14, 22, 36, 39, 40)
    ssid2  <- NULL
  } else {
    ssid <- ssids
    ssid2  <- ssids
  }


  # Initialize default tibble
  init <- tibble::tibble(
    fn = numeric(0),
    species = character(0),
    ssid = numeric(0)
  )

  # Initialize tibbles
  x <- init # Extra sets
  u <- init # Unlike values
  s <- init # Summary of returned
  a <- init # All sets when any unlike (x1, x2, a12)

  # Iterate over cases
  for (i in seq_along(spp)) {
    # Print current species
    cat(paste0("spp = ", spp[i], "\n"))
    # Reset tibbles
    d1_all <- NULL
    d2_all <- NULL
    d1e <- NULL
    d2e <- NULL
    d1_safe <- NULL
    d2_safe <- NULL
    # Safely
    safe_get_survey_sets <- purrr::safely(gfdata::get_survey_sets)
    safe_get_all_survey_sets <- purrr::safely(gfdata::get_all_survey_sets)

    if(length(get_args)==0){
      get_args <- list()
    }
    if(length(get_all_args)==0){
      get_all_args <- list()
    }

      get_args$species <- get_all_args$species <- spp[i]
      get_args$ssid <- ssid
      get_all_args$ssid <- ssid2


    # Get survey sets
    d1_safe <- do.call(safe_get_survey_sets, get_args)

    # Extract result and (first) error message
    d1_all <- d1_safe$result
    d1e <- d1_safe$error[[1]][1] # Extract first list element

    # Let server have a rest
    Sys.sleep(0.05)
    # Get all survey sets
    d2_safe <- do.call(safe_get_all_survey_sets, get_all_args)

    # Extract result and (first) error message
    d2_all <- d2_safe$result
    d2e <- d2_safe$error[[1]][1] # Extract first list element

    if(is.null(d1_all)){
      d1_all <- tibble::tibble(
        survey_series_id = numeric(0),
        survey_series_desc = character(0)
      )
    }
    if(is.null(d2_all)){
      d2_all <- tibble::tibble(
        survey_series_id = numeric(0),
        survey_series_desc = character(0)
      )
    }
    ssids_found <- dplyr::bind_rows(
      dplyr::select(d1_all, survey_series_id, survey_series_desc),
      dplyr::select(d2_all, survey_series_id, survey_series_desc)) |>
      dplyr::distinct()

    ssid <- unique(ssids_found$survey_series_id)

    for (j in seq_along(ssid)) {
      # Reset tibbles
      a12 <- NULL #
      d1 <- NULL
      d2 <- NULL
      dd <- NULL #
      s1 <- NULL #
      s2 <- NULL #
      x1 <- NULL #
      x2 <- NULL #
      # Reset vectors
      b <- NULL #
      n1 <- NULL #
      n2 <- NULL #
      r1 <- NULL #
      r2 <- NULL #
      r12 <- NULL #

      # Create comparison columns
      # - Robust to d1 <- NULL: Condition evaluates FALSE
      # - Robust to ncol(d1) == 0: Condition evaluates FALSE
      # - Robust to nrow(d1) == 0: Assigned value has nrow == 0
      #
      if (all(c("species_code", "fishing_event_id") %in% colnames(d1_all))) {
        d1 <- d1_all |>
          dplyr::filter(survey_series_id == ssid[j]) |>
          tidyr::drop_na(species_code, fishing_event_id) |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )

        if(drop_if_all_zero){
        # Drop all rows if all counts and weights each either zero or NA
        if (all(c(d1$catch_count, d1$catch_weight) %in% c(0, NA))) {
          d1 <- d1[0, ] # Drop all rows and keep columns
        }
       }

      }
      # Create comparison columns
      # - Robust to d2 <- NULL: Condition evaluates FALSE
      # - Robust to ncol(d2) == 0: Condition evaluates FALSE
      # - Robust to nrow(d2) == 0: Assigned value has nrow == 0
      #
      if (all(c("species_code", "fishing_event_id") %in% colnames(d2_all))) {
        d2 <- d2_all |>
          dplyr::filter(survey_series_id == ssid[j]) |>
          tidyr::drop_na(species_code, fishing_event_id) |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )
      }
      # Identify extra comparison_id
      n1 <- setdiff(d1$comparison_id, d2$comparison_id)
      n2 <- setdiff(d2$comparison_id, d1$comparison_id)
      # Identify shared comparison_id
      b <- dplyr::intersect(d1$comparison_id, d2$comparison_id)
      # New summary rows
      s1 <- tibble::tibble(
        fn = 1L,
        species = spp[i],
        ssid = ssid[j],
        survey = ssids_found$survey_series_desc[j],
        count_ids = ifelse(is.null(d1), NA, length(unique(d1$comparison_id))),
        extra_ids = ifelse(is.null(d1), NA, length(n1)),
        shared_ids = ifelse(is.null(d1), NA, length(b)),
        error = ifelse(is.null(d1e), "No", "Yes"),
        message = ifelse(is.null(d1e), "NULL", d1e)
      )
      s2 <- tibble::tibble(
        fn = 2L,
        species = spp[i],
        ssid = ssid[j],
        survey = ssids_found$survey_series_desc[j],
        count_ids = ifelse(is.null(d2), NA, length(unique(d2$comparison_id))),
        extra_ids = ifelse(is.null(d2), NA, length(n2)),
        shared_ids = ifelse(is.null(d2), NA, length(b)),
        error = ifelse(is.null(d2e), "No", "Yes"),
        message = ifelse(is.null(d2e), "NULL", d2e)

      )
      # Augment summary
      s <- dplyr::bind_rows(s, s1, s2)

      # New extra set rows
      if (length(n1) > 0) {
        # Extra set rows numbers
        r1 <- which(d1$comparison_id %in% n1)
        # Extra set tibble
        x1 <- tibble::tibble(
          fn = 1L,
          species = spp[i],
          ssid = ssid[j],
          d1[r1, ]
        )
      }
      if (length(n2) > 0) {
        # Extra set rows numbers
        r2 <- which(d2$comparison_id %in% n2)
        # Extra set tibble
        x2 <- tibble::tibble(
          fn = 2L,
          species = spp[i],
          ssid = ssid[j],
          d2[r2, ]
        )
      }
      # Augment extra sets
      x <- dplyr::bind_rows(x, x1, x2)

      # If either function had extra, include all shared as fn = 12
      any_d1 <- (length(unique(d1$comparison_id)) > 0)
      any_d2 <- (length(unique(d2$comparison_id)) > 0)
      any_extra <- ((length(n1) + length(n2)) > 0)
      if (any_d1 & any_d2 & any_extra) {
        # Shared event row numbers
        r12 <- which(!(d2$comparison_id %in% n2))
        a12 <- tibble::tibble(
          fn = 12L,
          species = spp[i],
          ssid = ssid[j],
          d2[r12, ]
        )
      }
      # Augment return all fishing events only when functions differed
      a <- dplyr::bind_rows(a, x1, x2, a12)

      # Only compares non-null output for both functions
      if (!is.null(d1) & !is.null(d2)) {
        # Only compare shared colnames
        cn <- intersect(colnames(d1), colnames(d2))
        # Shared columns
        d1 <- d1 |>
          dplyr::select(tidyselect::all_of(cn)) |>
          dplyr::mutate(fn = 1L, .before = 1)
        d2 <- d2 |>
          dplyr::select(tidyselect::all_of(cn)) |>
          dplyr::mutate(fn = 2L, .before = 1)
        # Bind rows
        dd <- dplyr::bind_rows(d1, d2) |>
          # Augment
          dplyr::mutate(species = spp[i], .before = 2L) |>
          dplyr::mutate(ssid = ssid[j], .before = 3L) |>
          # Round
          dplyr::mutate(
            dplyr::across(
              tidyselect::starts_with("density"), ~ round(.x, digits = 10)
            ),
            dplyr::across(
              tidyselect::starts_with("speed"), ~ round(.x, digits = 10)
            ),
            dplyr::across(
              tidyselect::starts_with("area"), ~ round(.x, digits = 8)
            )
          ) |>
          # Drop NAs
          tidyr::drop_na(species_code, fishing_event_id) |>
          # Arrange so same fishing_event_id are in sequential rows
          dplyr::arrange(species_code, fishing_event_id, fn) |>
          # Collapse to one row if equal (except in fn column)
          dplyr::distinct(dplyr::across(-fn), .keep_all = TRUE) |>
          dplyr::group_by(comparison_id) |>
          # Keep only groups with more than one row (the inconsistent groups)
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::ungroup()
      } # End if
      # Bind rows
      u <- dplyr::bind_rows(u, dd)
    }
  }

  # Arrange
  x <- dplyr::arrange(x, fn, species, ssid)
  s <- dplyr::arrange(s, species, ssid, fn)
  a <- dplyr::arrange(a, fn, species, ssid)

  # Return
  list(x = x, u = u, s = s, a = a)
}
