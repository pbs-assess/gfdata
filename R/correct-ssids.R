#' Custom fixes for problem surveys with shared trip ids resulting in assignment to wrong ssid
#'
#' @param dat df containing these columns: fishing_event_ids, survey_series_id, survey_id,
#'  major_stat_area_code, minor_stat_area_code
#' @param specimens Defaults to FALSE where checks for duplication of fishing_event_ids
#'
correct_ssids <- function(dat, specimens = FALSE){
  # first split data into unique fishing_events (dd1) and ones with duplicates (dd2)
# dat <- dat
#
# browser()

# # first only applying some fixes to duplicated fishing_events in case some are miss-assigned but not duplicated cases
# # for shrimp survey sets in both qcs and wcvi that were done on the same trip they get duplicated by the sql call
# # so drop all survey_ids for duplicated records because they are unreliable
# # note: getting some that violate these rules but aren't duplicated... eg. fe 1720260, 1720263

# if(!specimens) {
# .dd <- dat[duplicated(dat$fishing_event_id), ]
# # dd1 <- filter(dat, !(fishing_event_id %in% c(unique(.dd$fishing_event_id))))
# # dd2 <- filter(dat, (fishing_event_id %in% c(unique(.dd$fishing_event_id))))
# try(dat[dat$survey_series_id %in% c(6, 7) & (dat$fishing_event_id %in% c(unique(.dd$fishing_event_id))), ]$survey_id <- NA, silent = TRUE)
# try(dat[dat$survey_series_og %in% c(6, 7) & (dat$fishing_event_id %in% c(unique(.dd$fishing_event_id))), ]$survey_id <- NA, silent = TRUE)
#
# } else {
#   .dd <- dat[duplicated(dat$specimen_id), ]
#   # dd1 <- filter(dat, !(specimen_id %in% c(unique(.dd$specimen_id))))
#   # dd2 <- filter(dat, (specimen_id %in% c(unique(.dd$specimen_id))))
#
#   try(dat[dat$survey_series_id %in% c(6, 7) & (dat$specimen_id %in% c(unique(.dd$specimen_id))), ]$survey_id <- NA, silent = TRUE)
#   try(dat[dat$survey_series_og %in% c(6, 7) & (dat$specimen_id %in% c(unique(.dd$specimen_id))), ]$survey_id <- NA, silent = TRUE)
# }

# .d <- bind_rows(dd1, dd2)

### correct miss-assignment of survey series here

# MSSM
# try(dat[ ((dat$survey_series_id == 6 & dat$major_stat_area_code %in% c("03", "04"))), ]$survey_id <- NA, silent = TRUE)
# try(dat[ ((dat$survey_series_id == 7 & dat$major_stat_area_code %in% c("05", "06"))), ]$survey_id <- NA, silent = TRUE)

try(dat[dat$survey_series_id %in% c(6,7), ]$survey_id <- NA, silent = TRUE)
try(dat[ ((dat$survey_series_id == 6 & dat$major_stat_area_code %in% c("03", "04"))), ]$survey_series_id <- 7, silent = TRUE)
try(dat[ ((dat$survey_series_id == 7 & dat$major_stat_area_code %in% c("05", "06"))), ]$survey_series_id <- 6, silent = TRUE)
try(dat[ ((dat$survey_series_og == 6 & dat$major_stat_area_code %in% c("03", "04"))), ]$survey_series_og <- 7, silent = TRUE)
try(dat[ ((dat$survey_series_og == 7 & dat$major_stat_area_code %in% c("05", "06"))), ]$survey_series_og <- 6, silent = TRUE)

# SABLE doesn't work with SSIDs, use reason_desc and or grouping codes instead?
try(dat[dat$survey_series_id %in% c(35, 41, 42, 43), ]$survey_id <- NA, silent = TRUE) # this throws a warning when others don't
try(dat[dat$survey_series_id %in% c(35, 41, 42, 43), ]$survey_series_id <- 35, silent = TRUE)
try(dat[dat$survey_series_og %in% c(35, 41, 42, 43), ]$survey_series_og <- 35, silent = TRUE)
# try(dat[ ((dat$survey_series_id %in% c(35, 41, 42, 43) & dat$reason_desc == "EXPLORATORY")), ]$survey_series_id <- 35, silent = TRUE)
# try(dat[ ((dat$survey_series_id %in% c(35, 41, 42, 43) & dat$reason_desc == "SABLEFISH STANDARDIZED OFFSHORE SURVEY")), ]$survey_series_id <- 42, silent = TRUE)

# Jig surveys are split into too many separate survey series, so we'll assume all were assigned correctly and drop everything that doesn't match
dat <- dat[ (!(dat$survey_series_id == 82 & !(dat$minor_stat_area_code %in% c("12")))), ]
dat <- dat[ (!(dat$survey_series_id == 83 & !(dat$minor_stat_area_code %in% c("13")))), ]
dat <- dat[ (!(dat$survey_series_id == 84 & !(dat$minor_stat_area_code %in% c("15")))), ]
dat <- dat[ (!(dat$survey_series_id == 85 & !(dat$minor_stat_area_code %in% c("16")))), ]
dat <- dat[ (!(dat$survey_series_id == 86 & !(dat$minor_stat_area_code %in% c("18")))), ]
dat <- dat[ (!(dat$survey_series_id == 87 & !(dat$minor_stat_area_code %in% c("19")))), ]

# for IPHC station specific ssids, drop everything that doesn't match
dat <- dat[ (!(dat$survey_series_id == 17 & !(dat$minor_stat_area_code %in% c("3")))), ] # 3CD
dat <- dat[ (!(dat$survey_series_id == 18 & !(dat$minor_stat_area_code %in% c("6")))), ] # 5AB
dat <- dat[ (!(dat$survey_series_id == 19 & !(dat$minor_stat_area_code %in% c("7")))), ] # 5CD

dat |> dplyr::distinct()
}
