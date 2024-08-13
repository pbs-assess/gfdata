#' Custom fixes for problem surveys with shared trip ids resulting in assignment to wrong ssid
#'
#' @param dat df containing these columns: fishing_event_ids, survey_series_id, survey_id,
#'  major_stat_area_code, minor_stat_area_code
#'
#' @export
correct_ssid_errors <- function(dat){
  # first split data into unique fishing_events (dd1) and ones with duplicates (dd2)
.d <- dat
.dd <- .d[duplicated(.d$fishing_event_id), ]
dd1 <- filter(.d, !(fishing_event_id %in% c(unique(.dd$fishing_event_id))))
dd2 <- filter(.d, (fishing_event_id %in% c(unique(.dd$fishing_event_id))))

# then only applying fixes to duplicated fishing_events in case some are miss-assigned but not duplicated cases
# for shrimp survey sets in both qcs and wcvi that were done on the same trip they get duplicated by the sql call
# note: getting some that violate these rules but aren't duplicated... eg. fe 1720260, 1720263
dd2 <- dd2[ (!(dd2$survey_series_id == 6 & dd2$major_stat_area_code %in% c("03", "04"))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 7 & dd2$major_stat_area_code %in% c("05", "06"))), ]

# TODO: generate special cases for sablefish sets with inlets and offshore on same trip ???
# dd2 <- dd2[ (!(dd2$survey_series_id == ## & dd2$reason %in% c(""))),]
# dd2 <- dd2[ (!(dd2$survey_series_id == ## & dd2$reason %in% c(""))),]

# TODO: generate special cases for removing duplications of dogfish and HBLL on same trip ???

# Jig surveys
dd2 <- dd2[ (!(dd2$survey_series_id == 82 & !(dd2$minor_stat_area_code %in% c("12")))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 83 & !(dd2$minor_stat_area_code %in% c("13")))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 84 & !(dd2$minor_stat_area_code %in% c("15")))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 85 & !(dd2$minor_stat_area_code %in% c("16")))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 86 & !(dd2$minor_stat_area_code %in% c("18")))), ]
dd2 <- dd2[ (!(dd2$survey_series_id == 87 & !(dd2$minor_stat_area_code %in% c("19")))), ]

.d <- bind_rows(dd1, dd2)

### maybe we should correct remaining miss-assignment surveys here?
try(.d[ ((.d$survey_series_id == 6 & .d$major_stat_area_code %in% c("03", "04"))), ]$survey_id <- NA)
try(.d[ ((.d$survey_series_id == 7 & .d$major_stat_area_code %in% c("05", "06"))), ]$survey_id <- NA)
try(.d[ ((.d$survey_series_id == 6 & .d$major_stat_area_code %in% c("03", "04"))), ]$survey_series_id <- 7)
try(.d[ ((.d$survey_series_id == 7 & .d$major_stat_area_code %in% c("05", "06"))), ]$survey_series_id <- 6)

.d
}
