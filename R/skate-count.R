#' summarize sub/minor level counts from fishing event data at the parent event level
#' includes correction for a typo in dataframe
#' retrieves missing fishing_event_ids for sablefish surveys using major_level_ids
#'
#' @param fe df retrieved with get-event-data.sql
#'
get_parent_level_counts <- function(fe) {
  fe_A_no_parent <- filter(fe, is.na(FE_PARENT_EVENT_ID), is.na(FE_MINOR_LEVEL_ID), is.na(FE_SUB_LEVEL_ID)) # just actual events

  # browser()

  # get sub events (known as skates)
  fe_B_no_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), is.na(FE_MINOR_LEVEL_ID)) %>%
    select(FE_PARENT_EVENT_ID, FISHING_EVENT_ID, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    dplyr::distinct() %>%
    group_by(FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    mutate(SKATE_COUNT = n()) %>%
    rename(skate_id = FISHING_EVENT_ID, fishing_event_id = FE_PARENT_EVENT_ID) %>%
    ungroup()

  # get sub-sub events (usually hooks)
  fe_C_w_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), !is.na(FE_MINOR_LEVEL_ID))

  # correct a mistake where one hook 104 was given wrong FE_PARENT_EVENT_ID
  fe_C_w_minor["FE_PARENT_EVENT_ID"][fe_C_w_minor["FE_PARENT_EVENT_ID"] == 502596] <- 502717

  # fe_C_w_minor["FE_PARENT_EVENT_ID"][fe_C_w_minor["FE_PARENT_EVENT_ID"]==502717] <- 502596



  fe_C <- fe_C_w_minor %>%
    select(FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID, FE_MINOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    dplyr::distinct() %>%
    group_by(FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    mutate(
      MINOR_ID_COUNT = n(),
      #MINOR_ID_MAX = max(FE_MINOR_LEVEL_ID, na.rm = TRUE)
      MINOR_ID_MAX = ifelse(all(is.na(FE_MINOR_LEVEL_ID)), NA, max(FE_MINOR_LEVEL_ID, na.rm = TRUE))
    ) %>%
    select(-FE_MINOR_LEVEL_ID) |>
    dplyr::distinct() %>%
    mutate(skate_id = FE_PARENT_EVENT_ID) %>%
    ungroup()


  sub_event_counts <- full_join(
    fe_B_no_minor,
    fe_C
  )

  ## up to 220 skates, all sablefish 39, 41, or 43, are missing parent event ids
  missing_event_ids <- filter(sub_event_counts, is.na(fishing_event_id)) %>%
    select(-fishing_event_id) %>%
    left_join(select(fe_A_no_parent,
                     fishing_event_id = FISHING_EVENT_ID, FE_MAJOR_LEVEL_ID, TRIP_ID,
                     YEAR
    ))

  ## exploring missing ids
  # fe_A_no_parent[fe_A_no_parent$FISHING_EVENT_ID == 502717,]
  # fe_A_no_parent[fe_A_no_parent$SURVEY_ID == 65,] %>% View
  # fe_B_no_minor[fe_B_no_minor$FE_PARENT_EVENT_ID == 502717,]
  # fe_C_w_minor[fe_C_w_minor$FE_PARENT_EVENT_ID == 502717,] %>% View()
  # fe_C_w_minor[fe_C_w_minor$FE_PARENT_EVENT_ID == 502596,] %>% View()

  final_event_counts <- sub_event_counts %>%
    filter(!is.na(fishing_event_id)) %>%
    bind_rows(missing_event_ids) %>%
    group_by(fishing_event_id, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    # select(-SURVEY_ID, -SURVEY_SERIES_ID) %>%
    dplyr::distinct() %>%
    dplyr::summarise(
      skate_count = mean(SKATE_COUNT, na.rm = T),
      mean_per_skate = mean(MINOR_ID_COUNT, na.rm = T),
      minor_id_count = sum(MINOR_ID_COUNT, na.rm = T),
      # minor_id_max = max(MINOR_ID_MAX, na.rm = T)
      minor_id_max = ifelse(all(is.na(MINOR_ID_MAX)), NA, max(MINOR_ID_MAX, na.rm = TRUE))
    ) %>%
    mutate(diff = ifelse(minor_id_max > 0, minor_id_max - minor_id_count, NA)) %>%
    dplyr::distinct()

    fe2 <- fe_A_no_parent %>%
    rename(fishing_event_id = FISHING_EVENT_ID) %>%
    left_join(final_event_counts) %>%
    select(-FE_PARENT_EVENT_ID, -FE_MINOR_LEVEL_ID, -FE_SUB_LEVEL_ID)

  fe2 %>% dplyr::distinct()
}
