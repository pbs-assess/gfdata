d0 <- get_survey_sets(species = "424", ssid = c(22))
d1 <- get_all_survey_sets(species = "424",
                          # grouping_only = TRUE,
                          #quiet_option = "none",
                          ssid = c(22)
                          )

d0$fn <- 1
d1$fn <- 2


filter(d1, !(fishing_event_id %in% unique(d0$fishing_event_id))) |>
  relocate(fn, survey_series_id, survey_id, depth_m, usability_desc, grouping_desc, grouping_code, grouping_code_original, grouping_desc_original)|> View()

dc <- bind_rows(d0, d1) |> arrange(fishing_event_id) |>
  relocate(fn, survey_series_id, survey_id, depth_m, usability_desc, grouping_desc, grouping_code, grouping_code_original, grouping_desc_original)

dc |>
  dplyr::group_by(fishing_event_id) |>
  dplyr::distinct(
    dplyr::across(
      # dplyr::starts_with( # Ignore differences in these columns
        c(grouping_code
        )
      # )
    ),
    .keep_all = TRUE
  ) |>
  # Keep only groups with more than one row (the inconsistent groups)
  dplyr::filter(n() > 1) |>
  dplyr::ungroup() |>
  tibble::view()

dc |> filter(grouping_code %in% c(132, 133)) |> View()


ds <- get_survey_samples(species = "424", ssid = c(22))
ds2 <- get_all_survey_samples(species = "424",
                          # grouping_only = TRUE,
                          #quiet_option = "none",
                          include_event_info = TRUE,
                          ssid = c(22)
)

ds$fn <- 1
ds2$fn <- 2

filter(ds2, !(specimen_id %in% unique(ds$specimen_id))) |> View()

ds3 <- bind_rows(ds, ds2) |> arrange(specimen_id) |>
  relocate(fn, specimen_id, survey_series_id, survey_id, depth_begin, depth_end, usability_desc, grouping_desc, grouping_code, grouping_code_original, grouping_desc_original)

ds3 |>
  dplyr::group_by(specimen_id) |>
  dplyr::distinct(
    dplyr::across(
      # dplyr::starts_with( # Ignore differences in these columns
      c(grouping_code
      )
      # )
    ),
    .keep_all = TRUE
  ) |>
  # Keep only groups with more than one row (the inconsistent groups)
  dplyr::filter(n() > 1) |>
  dplyr::ungroup() |>
  tibble::view()




# d <- get_all_survey_samples(species = c(044, 394), major = c("03", "04","05", "06"), include_event_info = TRUE)

d <- get_all_survey_samples(species = "sablefish", ssid = c(35),
                            unsorted_only = TRUE,
                            random_only = TRUE,
                            grouping_only = TRUE,
                            remove_duplicates = TRUE,
                            include_event_info = TRUE)

d <- get_all_survey_samples(species = "044", ssid = c(6,7), include_event_info = TRUE)

d <- get_all_survey_samples(species = "044", ssid = c(48, 76, 92, 93), include_event_info = TRUE, quiet_option = "none")


d[duplicated(d$specimen_id),] |> View()
# dd <- filter(d, specimen_id == 7780677)


dset <- select(d1, fishing_event_id, fe_sub_level_id, catch_count) |> rename(set_count = catch_count)
dsam <- select(d, fishing_event_id, fe_sub_level_id, catch_count) |> distinct()
d2 <- left_join(dset, dsam) |> mutate(ratio = catch_count/set_count)
