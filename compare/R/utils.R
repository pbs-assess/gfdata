

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
