
d1 <- get_all_survey_sets(species = "044", ssid = c(48, 76, 92, 93), quiet_option = "none")


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
