# Source utils-compare.R
source(here::here("compare", "R", "utils-compare.R"))

# Compare survey samples
compare_survey_samples <- function (spp, ssid, areas = NULL) {

  # Initialize tibbles
  x <- tibble::tibble() # Extra specimens
  u <- tibble::tibble() # Unlike values
  s <- tibble::tibble() # Summary of returned

  # Iterate over cases
  for (i in seq_along(spp)) {
    for (j in seq_along(ssid)) {
      # Print current pair
      cat(paste0("spp = ", spp[i], "; ssid = ", ssid[j], "\n"))
      # Reset tibbles
      d1 <- NULL
      d2 <- NULL
      dd <- NULL
      s1 <- NULL
      s2 <- NULL
      x1 <- NULL
      x2 <- NULL
      # Resent vectors
      b <- NULL
      n1 <- NULL
      n2 <- NULL
      r1 <- NULL
      r2 <- NULL
      # Pull data
      try(d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ssid[j]), major = areas)
      try(d2 <- gfdata::get_all_survey_samples(species = spp[i], ssid = ssid[j]), major = areas)
      # Drop NA specimen_id
      if (!is.null(d1)) {
        d1 <- d1 |> tidyr::drop_na(specimen_id)
      }
      if (!is.null(d2)) {
        d2 <- d2 |> tidyr::drop_na(specimen_id)
      }
      # Identify extra specimen_id
      n1 <- setdiff(d1$specimen_id, d2$specimen_id)
      n2 <- setdiff(d2$specimen_id, d1$specimen_id)
      # Identify shared specimen_id
      b <- dplyr::intersect(d1$specimen_id, d2$specimen_id)
      # New summary rows
      s1 <- tibble::tibble(
        fn = 1L,
        species = spp[i],
        ssid = ssid[j],
        survey = survey(ssid),
        count_ids = ifelse(is.null(d1), NA, length(unique(d1$specimen_id))),
        extra_ids = ifelse(is.null(d1), NA, length(n1)),
        shared_ids = ifelse(is.null(d1), NA, length(b))
      )
      s2 <- tibble::tibble(
        fn = 2L,
        species = spp[i],
        ssid = ssid[j],
        survey = survey(ssid),
        count_ids = ifelse(is.null(d2), NA, length(unique(d2$specimen_id))),
        extra_ids = ifelse(is.null(d2), NA, length(n2)),
        shared_ids = ifelse(is.null(d2), NA, length(b))
      )
      # Augment summary
      s <- dplyr::bind_rows(s, s1, s2)

      # New extra specimen rows
      if (length(n1) > 0) {
        # Extra specimen rows numbers
        r1 <- which(d1$specimen_id %in% n1)
        # Extra specimen tibbles
        x1 <- tibble::tibble(
          fn = 1L,
          species = spp[i],
          ssid = ssid[j],
          d1[r1, ]
        )
      }
      if (length(n2) > 0) {
        # Extra specimen rows numbers
        r2 <- which(d2$specimen_id %in% n2)
        x2 <- tibble::tibble(
          fn = 2L,
          species = spp[i],
          ssid = ssid[j],
          d2[r2, ]
        )
      }
      x3 <- NULL
      r3 <- NULL
      # If either function had extra, include the ones that were shared as fn = 12
      if ((length(n1) + length(n2)) > 0 & length(unique(d2$specimen_id)) > 0) {
        # Shared specimen rows numbers
        r3 <- which(!(d2$specimen_id %in% n2))
        x3 <- tibble::tibble(
          fn = 12L,
          species = spp[i],
          ssid = ssid[j],
          d2[r3, ]
        )
      }
      # Augment return all specimens only when functions differed
      x <- dplyr::bind_rows(x, x1, x2, x3)


      # Only compares non-null output for both functions
      if (!is.null(d1) & !is.null(d2)) {
        # Only compare shared colnames
        cn <- intersect(colnames(d1), colnames(d2))
        # Shared columns
        d1 <- d1 |> select(all_of(cn)) |> mutate(fn = 1L, .before = 1)
        d2 <- d2 |> select(all_of(cn)) |> mutate(fn = 2L, .before = 1)
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
      u <- dplyr::bind_rows(u, dd)
    }
  }

  # Arrange
  x <- dplyr::arrange(x, species, ssid, fn)
  s <- dplyr::arrange(s, species, ssid, fn)

  # Return
  list(x = x, u = u, s = s)
}
