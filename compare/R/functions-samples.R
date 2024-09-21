# Source utils-compare.R
source(here::here("compare", "R", "utils-compare.R"))

# Compare survey samples
compare_survey_samples <- function (spp, ssid, areas = NULL) {

  # Initialize default tibble
  init <- tibble::tibble(
    fn = numeric(0),
    species = character(0),
    ssid = numeric(0)
  )

  # Initialize tibbles
  x <- init # Extra specimens
  u <- init # Unlike values
  s <- init # Summary of returned
  a <- init # All specimens when any unlike (x1, x2, a12)

  # Iterate over cases
  for (i in seq_along(spp)) {
    for (j in seq_along(ssid)) {
      # Print current pair
      cat(paste0("spp = ", spp[i], "; ssid = ", ssid[j], "\n"))
      # Reset tibbles
      a12 <- NULL
      d1 <- NULL
      d2 <- NULL
      d1e <- NULL
      d2e <- NULL
      d1_safe <- NULL
      d2_safe <- NULL
      dd <- NULL
      s1 <- NULL
      s2 <- NULL
      x1 <- NULL
      x2 <- NULL
      # Reset vectors
      b <- NULL
      n1 <- NULL
      n2 <- NULL
      r1 <- NULL
      r2 <- NULL
      r12 <- NULL
      # Safely
      safe_get_survey_samples <- purrr::safely(
        gfdata::get_survey_samples
      )
      safe_get_all_survey_samples <- purrr::safely(
        gfdata::get_all_survey_samples
      )
      # Get survey samples
      d1_safe <- safe_get_survey_samples(
        species = spp[i],
        ssid = ssid[j],
        major = areas
      )
      # Extract result and (first) error message
      d1 <- d1_safe$result
      d1e <- d1_safe$error[[1]][1] # Extract first list element
      # Let server have a rest
      Sys.sleep(0.05)
      # Get all survey samples
      d2_safe <- safe_get_all_survey_samples(
        species = spp[i],
        ssid = ssid[j],
        major = areas,
        unsorted_only = TRUE,
        random_only = TRUE,
        grouping_only = TRUE,
        remove_duplicates = TRUE,
        include_event_info = TRUE
      )
      # Extract result and (first) error message
      d2 <- d2_safe$result
      d2e <- d2_safe$error[[1]][1] # Extract first list element

      # # Pull data
      # try(
      #   d1 <- gfdata::get_survey_samples(
      #     species = spp[i],
      #     ssid = ssid[j],
      #     major = areas
      #   )
      # )
      # # Let server have a rest
      # Sys.sleep(0.05)
      # try(
      #   d2 <- gfdata::get_all_survey_samples(
      #     species = spp[i],
      #     ssid = ssid[j],
      #     major = areas,
      #     unsorted_only = TRUE,
      #     random_only = TRUE,
      #     grouping_only = TRUE,
      #     remove_duplicates = TRUE,
      #     include_event_info = TRUE
      #   )
      # )

      # # Set to null if not data frame or if no columns
      # if (is.data.frame(d1)) {
      #   if (ncol(d1) == 0) {
      #     d1 <- NULL
      #   }
      # } else {
      #   d1 <- NULL
      # }
      # if (is.data.frame(d2)) {
      #   if (ncol(d2) == 0) {
      #     d2 <- NULL
      #   }
      # } else {
      #   d2 <- NULL
      # }

      # Drop NA specimen_id
      if ("specimen_id" %in% colnames(d1)) {
        d1 <- d1 |> tidyr::drop_na(specimen_id)
      }
      if ("specimen_id" %in% colnames(d2)) {
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
        shared_ids = ifelse(is.null(d1), NA, length(b)),
        error = ifelse(is.null(d1e), "No", "Yes"),
        message = ifelse(is.null(d1e), "NULL", d1e)
      )
      s2 <- tibble::tibble(
        fn = 2L,
        species = spp[i],
        ssid = ssid[j],
        survey = survey(ssid),
        count_ids = ifelse(is.null(d2), NA, length(unique(d2$specimen_id))),
        extra_ids = ifelse(is.null(d2), NA, length(n2)),
        shared_ids = ifelse(is.null(d2), NA, length(b)),
        error = ifelse(is.null(d2e), "No", "Yes"),
        message = ifelse(is.null(d2e), "NULL", d2e)
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
      # Augment extra specimens
      x <- dplyr::bind_rows(x, x1, x2)

      # If either function had extra, include all shared as fn = 12
      if ((length(n1) + length(n2)) > 0 & length(unique(d2$specimen_id)) > 0) {
        # Shared specimen rows numbers
        r12 <- which(!(d2$specimen_id %in% n2))
        a12 <- tibble::tibble(
          fn = 12L,
          species = spp[i],
          ssid = ssid[j],
          d2[r12, ]
        )
      }
      # Augment all specimens only when functions differed
      a <- dplyr::bind_rows(a, x1, x2, a12)

      # Only compares non-null output for both functions
      if (!is.null(d1) & !is.null(d2)) {
        # Only compare shared colnames
        cn <- intersect(colnames(d1), colnames(d2))
        # Shared columns
        d1 <- d1 |> select(all_of(cn)) |> mutate(fn = 1L, .before = 1)
        d2 <- d2 |> select(all_of(cn)) |> mutate(fn = 2L, .before = 1)
        # Bind rows
        dd <- bind_rows(d1, d2) |>
          # Augment
          dplyr::mutate(species = spp[i], .before = 2L) |>
          dplyr::mutate(ssid = ssid[j], .before = 3L) |>
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
  x <- dplyr::arrange(x, fn, species, ssid)
  s <- dplyr::arrange(s, species, ssid, fn)
  a <- dplyr::arrange(a, fn, species, ssid)

  # Return
  list(x = x, u = u, s = s, a = a)
}
