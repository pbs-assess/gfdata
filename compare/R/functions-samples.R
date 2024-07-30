

compare_specimens <- function (spp, ssid) {

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
      try(d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ssid[j]))
      try(d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ssid[j]))
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
      # Identify extra specimen_id
      n1 <- setdiff(d1$specimen_id, d2$specimen_id)
      n2 <- setdiff(d2$specimen_id, d1$specimen_id)
      # Identify and store extra specimen_id rows
      if (length(n1) > 0) {
        r1 <- which(d1$specimen_id %in% n1)
        s1 <- rbind(s1, tibble::tibble(spp = spp[i], ssid = ssid[j], d1[r1, ]))
      }
      if (length(n2 > 0)) {
        r2 <- which(d2$specimen_id %in% n2)
        s2 <- rbind(s2, tibble::tibble(spp = spp[i], ssid = ssid[j], d2[r2, ]))
      }
    }
  }

  # Remove NAs (known issue)
  s1 <- s1 |> tidyr::drop_na(specimen_id)

  # Return
  list(e1 = e1, e2 = e2, s1 = s1, s2 = s2)
}

compare_specimen_values <- function (spp, ssid) {

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
      try(d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ssid[j]))
      try(d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ssid[j]))
      # Only compares non-null output for both functions
      if (!is.null(d1) & !is.null(d2)) {
        # Only compare shared colnames
        cn <- intersect(colnames(d1), colnames(d2))
        # Shared columns
        d1 <- d1 |> select(all_of(cn)) |> mutate(fn = "d1", .before = 1)
        d2 <- d2 |> select(all_of(cn)) |> mutate(fn = "d2", .before = 1)
        # Bind rows
        dd <- bind_rows(d1, d2) |>
          # Drop dogfish NAs
          tidyr::drop_na(specimen_id) |>
          # Arrange so same specimen_ids are in sequential rows
          dplyr::arrange(fishing_event_id, sample_id, specimen_id, fn) |>
          # Hack to fix different value case
          dplyr::mutate(length_type = tolower(length_type)) |>
          # Collapse to one row if equal (except in fn column)
          dplyr::distinct(dplyr::across(-fn), .keep_all = TRUE) |>
          dplyr::group_by(specimen_id) |>
          # Keep only groups with more than one row (the inconsistent groups)
          dplyr::filter(n() > 1) |>
          dplyr::ungroup()
      } # End if
      # Bind rows
      d <- rbind(d, dd)
    }
  }
  # Return
  d
}
