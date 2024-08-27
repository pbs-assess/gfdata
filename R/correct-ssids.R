#' Custom fixes for problem surveys with shared trip ids resulting in assignment to wrong ssid
#'
#' @param dat df containing these columns: fishing_event_ids, survey_series_id, survey_id,
#'  major_stat_area_code, minor_stat_area_code
#' @param specimens Defaults to FALSE where checks for duplication of fishing_event_ids
#'
correct_ssids <- function(dat, specimens = FALSE){
  # first split data into unique fishing_events (dd1) and ones with duplicates (dd2)
.d <- dat

if(!specimens) {
.dd <- .d[duplicated(.d$fishing_event_id), ]
dd1 <- filter(.d, !(fishing_event_id %in% c(unique(.dd$fishing_event_id))))
dd2 <- filter(.d, (fishing_event_id %in% c(unique(.dd$fishing_event_id))))

dd2 <- dd2 |> group_by(fishing_event_id) |>
  mutate(grouping_code = mean(grouping_code, na.rm = TRUE),
         grouping_code = ifelse(is.nan(grouping_code), NA, grouping_code)
         ) |> dplyr::distinct() |> ungroup()

} else {
  .dd <- .d[duplicated(.d$specimen_id), ]
  dd1 <- filter(.d, !(specimen_id %in% c(unique(.dd$specimen_id))))
  dd2 <- filter(.d, (specimen_id %in% c(unique(.dd$specimen_id))))

  dd2 <- dd2 |> group_by(specimen_id) |>
    mutate(grouping_code = mean(grouping_code, na.rm = TRUE),
           grouping_code = ifelse(is.nan(grouping_code), NA, grouping_code)
           ) |> dplyr::distinct() |> ungroup()
}

# then only applying fixes to duplicated fishing_events in case some are miss-assigned but not duplicated cases
# for shrimp survey sets in both qcs and wcvi that were done on the same trip they get duplicated by the sql call
# note: getting some that violate these rules but aren't duplicated... eg. fe 1720260, 1720263
dd2 <- dd2[ (!(dd2$survey_series_id == 6 & dd2$major_stat_area_code %in% c("03", "04"))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 7 & dd2$major_stat_area_code %in% c("05", "06"))), ]

# and drop all survey_ids because they are unreliable
try(dd2[dd2$survey_series_id %in% c(6, 7), ]$survey_id <- NA, silent = TRUE)


# TODO: generate special cases for sablefish sets with inlets and offshore on same trip ???
# dd2 <- dd2[ (!(dd2$survey_series_id == 41 & dd2$reason_desc %in% c("SABLEFISH STANDARDIZED OFFSHORE SURVEY","SABLEFISH RANDOM STRATIFIED SURVEY"))),]
# dd2 <- dd2[ (!(dd2$survey_series_id %in% c(43) & dd2$reason_desc %in% c("SABLEFISH STANDARDIZED OFFSHORE SURVEY"))),]
# dd2 <- dd2[ (!(dd2$survey_series_id %in% c(42,43) & dd2$reason_desc %in% c("SABLEFISH STANDARDIZED INLET SURVEY"))),]

#
# # Jig surveys - moved to apply to all samples rather than just duplicated ones.
# dd2 <- dd2[ (!(dd2$survey_series_id == 82 & !(dd2$minor_stat_area_code %in% c("12")))), ]
# dd2 <- dd2[ (!(dd2$survey_series_id == 83 & !(dd2$minor_stat_area_code %in% c("13")))), ]
# dd2 <- dd2[ (!(dd2$survey_series_id == 84 & !(dd2$minor_stat_area_code %in% c("15")))), ]
# dd2 <- dd2[ (!(dd2$survey_series_id == 85 & !(dd2$minor_stat_area_code %in% c("16")))), ]
# dd2 <- dd2[ (!(dd2$survey_series_id == 86 & !(dd2$minor_stat_area_code %in% c("18")))), ]
# dd2 <- dd2[ (!(dd2$survey_series_id == 87 & !(dd2$minor_stat_area_code %in% c("19")))), ]

.d <- bind_rows(dd1, dd2)

### maybe we should correct remaining miss-assignment surveys here?

# MSSM
try(.d[ ((.d$survey_series_id == 6 & .d$major_stat_area_code %in% c("03", "04"))), ]$survey_id <- NA, silent = TRUE)
try(.d[ ((.d$survey_series_id == 7 & .d$major_stat_area_code %in% c("05", "06"))), ]$survey_id <- NA, silent = TRUE)
try(.d[ ((.d$survey_series_id == 6 & .d$major_stat_area_code %in% c("03", "04"))), ]$survey_series_id <- 7, silent = TRUE)
try(.d[ ((.d$survey_series_id == 7 & .d$major_stat_area_code %in% c("05", "06"))), ]$survey_series_id <- 6, silent = TRUE)

# SABLE doesn't work with SSIDs, use reason_desc and or grouping codes instead?
try(.d[.d$survey_series_id %in% c(35, 41, 42, 43), ]$survey_id <- NA, silent = TRUE) # this throws a warning when others don't
try(.d[.d$survey_series_id %in% c(35, 41, 42, 43), ]$survey_series_id <- 35, silent = TRUE)
# try(.d[ ((.d$survey_series_id %in% c(35, 41, 42, 43) & .d$reason_desc == "EXPLORATORY")), ]$survey_series_id <- 35, silent = TRUE)
# try(.d[ ((.d$survey_series_id %in% c(35, 41, 42, 43) & .d$reason_desc == "SABLEFISH STANDARDIZED OFFSHORE SURVEY")), ]$survey_series_id <- 42, silent = TRUE)

# Jig surveys are split into too many separate survey series, so we'll assume all were assigned correctly and drop everything that doesn't match
.d <- .d[ (!(.d$survey_series_id == 82 & !(.d$minor_stat_area_code %in% c("12")))), ]
.d <- .d[ (!(.d$survey_series_id == 83 & !(.d$minor_stat_area_code %in% c("13")))), ]
.d <- .d[ (!(.d$survey_series_id == 84 & !(.d$minor_stat_area_code %in% c("15")))), ]
.d <- .d[ (!(.d$survey_series_id == 85 & !(.d$minor_stat_area_code %in% c("16")))), ]
.d <- .d[ (!(.d$survey_series_id == 86 & !(.d$minor_stat_area_code %in% c("18")))), ]
.d <- .d[ (!(.d$survey_series_id == 87 & !(.d$minor_stat_area_code %in% c("19")))), ]

# for IPHC station specific ssids, drop everything that doesn't match
.d <- .d[ (!(.d$survey_series_id == 17 & !(.d$minor_stat_area_code %in% c("3")))), ] # 3CD
.d <- .d[ (!(.d$survey_series_id == 18 & !(.d$minor_stat_area_code %in% c("6")))), ] # 5AB
.d <- .d[ (!(.d$survey_series_id == 19 & !(.d$minor_stat_area_code %in% c("7")))), ] # 5CD

.d |> dplyr::distinct()
}
