compare_sets <- function (spp, ssid) {

  # Extra specimens
  s1 <- tibble::tibble()
  s2 <- tibble::tibble()

  # Error tibbles
  e1 <- tibble(returned = character(0), spp = character(0), ssid = numeric(0))
  e2 <- tibble(returned = character(0), spp = character(0), ssid = numeric(0))

  # Iterate over cases
  for (i in seq_along(spp)) {
    for (j in seq_along(ssid)) {
      # Print current pair
      cat(paste0("spp = ", spp[i], "; ssid = ", ssid[j], "\n"))
      # Reset value
      d1 <- NULL
      d2 <- NULL
      # Pull data
      try(d1 <- gfdata::get_survey_sets(species = spp[i], ssid = ssid[j]))
      try(d2 <- gfdata::get_survey_sets2(species = spp[i], ssid = ssid[j]))
      # Check value
      e1 <- rbind(
        e1,
        tibble(
          returned = ifelse(!is.null(d1), "yes", "no"),
          spp = spp[i],
          ssid = ssid[j]
        )
      )
      e2 <- rbind(
        e2,
        tibble(
          returned = ifelse(!is.null(d2), "yes", "no"),
          spp = spp[i],
          ssid = ssid[j]
        )
      )
      # Reset value
      n1 <- NULL
      n2 <- NULL
      # Create comparison columns
      if (!is.null(d1)) {
        d1 <- d1 |>
          tidyr::drop_na(species_code, fishing_event_id) |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )
      }
      if (!is.null(d2)) {
        d2 <- d2 |>
          tidyr::drop_na(species_code, fishing_event_id) |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )
      }
      # Identify extra comparison_id
      n1 <- setdiff(d1$comparison_id, d2$comparison_id)
      n2 <- setdiff(d2$comparison_id, d1$comparison_id)
      # Identify and store extra comparison_id rows
      if (length(n1) > 0) {
        r1 <- which(d1$comparison_id %in% n1)
        # Bind rows - Should accept different sets of columns
        s1 <- dplyr::bind_rows(
          s1,
          tibble::tibble(spp = spp[i], ssid = ssid[j], d1[r1, ])
        )
      }
      if (length(n2 > 0)) {
        r2 <- which(d2$comparison_id %in% n2)
        # Bind rows - Should accept different sets of columns
        s2 <- dplyr::bind_rows(
          s2,
          tibble::tibble(spp = spp[i], ssid = ssid[j], d2[r2, ])
        )
      }
    }
  }

  # Remove NAs (known issue)
  # s1 <- s1 |> tidyr::drop_na(comparison_id)

  # Return
  list(e1 = e1, e2 = e2, s1 = s1, s2 = s2)
}

compare_set_values <- function (spp, ssid) {

  # Initialize results
  d <- tibble::tibble()

  # Iterate over cases
  for (i in seq_along(spp)) {
    for (j in seq_along(ssid)) {
      # Print current pair
      cat(paste0("spp = ", spp[i], "; ssid = ", ssid[j], "\n"))
      # Reset value
      d1 <- NULL
      d2 <- NULL
      dd <- NULL
      # Pull data
      try(d1 <- gfdata::get_survey_sets(species = spp[i], ssid = ssid[j]))
      try(d2 <- gfdata::get_survey_sets2(species = spp[i], ssid = ssid[j]))
      # Only compares non-null output for both functions
      if (!is.null(d1) & !is.null(d2)) {
        # Create comparison columns
        d1 <- d1 |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )
        d2 <- d2 |>
          dplyr::mutate(
            comparison_id = paste0(species_code, fishing_event_id),
            .before = 1
          )
        # Only compare shared colnames
        cn <- intersect(colnames(d1), colnames(d2))
        # Shared columns
        d1 <- d1 |> select(all_of(cn)) |> mutate(fn = "d1", .before = 1)
        d2 <- d2 |> select(all_of(cn)) |> mutate(fn = "d2", .before = 1)
        # Bind rows
        dd <- dplyr::bind_rows(d1, d2) |>
          # Drop NAs
          tidyr::drop_na(species_code, fishing_event_id) |>
          # Arrange so same fishing_event_id are in sequential rows
          dplyr::arrange(species_code, fishing_event_id, fn) |>
          # Remove small differences in density (<< 1e-03)
          mutate(across(starts_with("density_"), \(x) round(x, 3))) |>
          # Collapse to one row if equal (except in fn column)
          dplyr::distinct(dplyr::across(-fn), .keep_all = TRUE) |>
          dplyr::group_by(comparison_id) |>
          # Keep only groups with more than one row (the inconsistent groups)
          dplyr::filter(n() > 1) |>
          dplyr::ungroup()
      } # End if
      # Bind rows
      d <- dplyr::bind_rows(d, dd)
    }
  }
  # Return
  d
}
