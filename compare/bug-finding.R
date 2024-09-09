

# d <- get_all_survey_samples(species = c(044, 394), major = c("03", "04","05", "06"), include_event_info = TRUE)

d <- get_all_survey_samples(species = "sablefish", ssid = c(35),
                            unsorted_only = TRUE,
                            random_only = TRUE,
                            grouping_only = TRUE,
                            remove_duplicates = TRUE,
                            include_event_info = TRUE)

d <- get_all_survey_samples(species = "044", ssid = c(6,7), include_event_info = TRUE)


d[duplicated(d$specimen_id),] |> View()
# dd <- filter(d, specimen_id == 7780677)
