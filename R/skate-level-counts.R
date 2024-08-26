#' summarize fishing event data at the skate level
#' retrieves missing fishing_event_ids for sablefish surveys using major_level_ids
#'

get_skate_level_counts <- function(fe) {

  fe <-  fe |> distinct()

  fe_A_no_parent <- filter(fe, is.na(FE_PARENT_EVENT_ID), is.na(FE_MINOR_LEVEL_ID), is.na(FE_SUB_LEVEL_ID)) |>
    select(-FE_PARENT_EVENT_ID, -FE_SUB_LEVEL_ID, -FE_MINOR_LEVEL_ID) |>
    rename(fishing_event_id = FISHING_EVENT_ID) # just actual events

  # get sub events (known as skates)
  # when present hook data is stored at this level, while other event info tends to be stored at the parent event level
  fe_B_no_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), is.na(FE_MINOR_LEVEL_ID)) %>%
    select(FE_PARENT_EVENT_ID, FISHING_EVENT_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID,
           SURVEY_SERIES_ID,
           YEAR, TRIP_ID, HOOK_CODE, LGLSP_HOOK_COUNT, HOOK_DESC, HOOKSIZE_DESC) %>%
    dplyr::distinct() %>%
    group_by(YEAR, TRIP_ID, FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID) %>%
    mutate(SKATE_COUNT = n()) %>%
    rename(skate_id = FISHING_EVENT_ID, fishing_event_id = FE_PARENT_EVENT_ID) %>%
    ungroup()

  # select all actual events that lack sub levels
  fe_without_B <- fe_A_no_parent |>
    anti_join(fe_B_no_minor, by = "fishing_event_id")

  # ### old version
  # fe_with_hook <- fe |> rename(fishing_event_id = FISHING_EVENT_ID) |>
  #   select(-SURVEY_SERIES_ID)|>
  #   filter(!is.na(HOOK_CODE))
  #
  #
  # fe_A_data_for_B <- fe |> rename(fishing_event_id = FISHING_EVENT_ID) |>
  #   filter(is.na(HOOK_CODE)) |>
  #   select(
  #     -FE_PARENT_EVENT_ID, -FE_MAJOR_LEVEL_ID, -FE_SUB_LEVEL_ID,
  #     -YEAR, -TRIP_ID, -SURVEY_SERIES_ID,
  #     -HOOK_CODE, -LGLSP_HOOK_COUNT, -HOOK_DESC, -HOOKSIZE_DESC
  #   )
  #
  # fe_with_B_and_hook <- fe_B_no_minor |> left_join(fe_A_data_for_B) |> filter(!is.na(HOOK_CODE))
  #
  # fe_with_B_no_hook <- fe_B_no_minor |> filter(is.na(HOOK_CODE))|>
  #   select(
  #     -FE_MAJOR_LEVEL_ID, -FE_SUB_LEVEL_ID,
  #     # -YEAR, -TRIP_ID,
  #     # -SURVEY_SERIES_ID,
  #     -HOOK_CODE, -LGLSP_HOOK_COUNT, -HOOK_DESC, -HOOKSIZE_DESC
  #   ) |> left_join(fe_with_hook)
  #
  #
  # fe_by_event_or_skate <- bind_rows(fe_without_B, fe_with_B_and_hook) |>
  #   bind_rows(fe_with_B_no_hook) |>
  #   select(-FE_PARENT_EVENT_ID, -FE_MINOR_LEVEL_ID)

  ## new version
  # sublevel missing hook info, needs all parent event level covariates
  # there also seems to be some disagreement between levels and the SSID assigned,
  # so using parent level when hook code unknown at skate level
  # fe_with_B_no_hook <- fe_B_no_minor[which(fe_B_no_minor$HOOK_CODE %in% 0 | is.na(fe_B_no_minor$HOOK_CODE)),]
  fe_with_B_no_hook <- fe_B_no_minor |> filter(is.na(HOOK_CODE)|HOOK_CODE == 0) |>
    select(-HOOK_CODE, -LGLSP_HOOK_COUNT, -HOOK_DESC, -HOOKSIZE_DESC, -SURVEY_SERIES_ID) |>
    left_join(fe_A_no_parent)

  # sublevel w hook info, needs all parent event covariates except the hook ones and SSID
  # using sub level SSID when hook code IS known at skate level
  fe_A_data_no_hook <- fe_A_no_parent |>
    select(-HOOK_CODE, -LGLSP_HOOK_COUNT, -HOOK_DESC, -HOOKSIZE_DESC, -SURVEY_SERIES_ID) |>
    distinct()

  # fe_with_B_and_hook <- fe_B_no_minor[which(fe_B_no_minor$HOOK_CODE %in% c(1,3)),]
  fe_with_B_and_hook <- fe_B_no_minor |> filter(HOOK_CODE != 0) |>
    left_join(fe_A_data_no_hook)

  # # this sometimes adds up to more than what we started with because of survey or grouping code duplications
  # nrow(fe_with_B_no_hook) +
  #   nrow(fe_with_B_and_hook) >= nrow(fe_B_no_minor)

  fe_by_event_or_skate <- bind_rows(fe_without_B, fe_with_B_and_hook) |>
    bind_rows(fe_with_B_no_hook)

  # # get sub-sub events (usually hooks)
  # # fe_C <- filter(fe_w_parent_events, !is.na(FE_MINOR_LEVEL_ID))
  fe_C_w_minor <- filter(fe, !is.na(FE_PARENT_EVENT_ID), !is.na(FE_MINOR_LEVEL_ID))

  # correct a mistake where one hook 104 was given wrong FE_PARENT_EVENT_ID
  fe_C_w_minor["FE_PARENT_EVENT_ID"][fe_C_w_minor["FE_PARENT_EVENT_ID"] == 502596] <- 502717

  # fe_C_w_minor["FE_PARENT_EVENT_ID"][fe_C_w_minor["FE_PARENT_EVENT_ID"]==502717] <- 502596

  fe_C <- fe_C_w_minor %>%
    select(FE_MINOR_LEVEL_ID, FE_SUB_LEVEL_ID, FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    dplyr::distinct() %>%
    group_by(FE_SUB_LEVEL_ID, FE_PARENT_EVENT_ID, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    mutate(
      MINOR_ID_COUNT = n(),
      #MINOR_ID_MAX = max(FE_MINOR_LEVEL_ID, na.rm = TRUE)
      MINOR_ID_MAX = ifelse(all(is.na(FE_MINOR_LEVEL_ID)), NA, max(FE_MINOR_LEVEL_ID, na.rm = TRUE))
    ) %>%
    select(-FE_MINOR_LEVEL_ID) %>%
    dplyr::distinct() %>%
    mutate(skate_id = FE_PARENT_EVENT_ID) %>%
    ungroup()

  # fe_C_w_minor["FE_PARENT_EVENT_ID"][fe_C_w_minor["FE_PARENT_EVENT_ID"]== 502596]

  sub_event_counts <- full_join(
    fe_B_no_minor,
    fe_C
  )

  ## up to 220 skates, all sablefish 39, 41, or 43, are missing parent event ids
  missing_event_ids <- filter(sub_event_counts, is.na(fishing_event_id)) %>%
    select(-fishing_event_id) %>%
    left_join(select(fe_A_no_parent,
                     fishing_event_id, FE_MAJOR_LEVEL_ID, TRIP_ID,
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
    dplyr::distinct() %>%
    group_by(skate_id, FE_SUB_LEVEL_ID, fishing_event_id, FE_MAJOR_LEVEL_ID, YEAR, TRIP_ID) %>%
    dplyr::summarise(
      # skate_count = mean(SKATE_COUNT, na.rm = T),
      mean_per_skate = mean(MINOR_ID_COUNT, na.rm = T),
      minor_id_count = sum(MINOR_ID_COUNT, na.rm = T),
      # minor_id_max = max(MINOR_ID_MAX, na.rm = T)
      minor_id_max = ifelse(all(is.na(MINOR_ID_MAX)), NA, max(MINOR_ID_MAX, na.rm = TRUE))
    ) %>%
    dplyr::distinct() %>%
    mutate(diff = ifelse(minor_id_max > 0, minor_id_max - minor_id_count, NA))

  fe2 <- fe_by_event_or_skate |>
    left_join(final_event_counts) |>
    # select(-SURVEY_SERIES_ID)|>
    # select(-FE_PARENT_EVENT_ID, -FE_MINOR_LEVEL_ID) %>%
    dplyr::distinct()

  fe2
}
