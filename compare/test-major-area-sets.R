### test major areas filter on survey sets

spp <- "North Pacific Spiny Dogfish"

## all canadian waters and unknown
# major_areas <- c("0","00","01", "02", "03", "04", "05", "06", "07", "08", "09", "11",
#                  "71","72","73","74","75","76","77","99")

major_areas <- c("01", "68") # SOG and Puget Sound

d <-get_all_survey_sets(species = spp, ssid = NULL, major = major_areas)

x <- d1# |> filter(is.na(skate_count)|skate_count == 1)
dd <- x[duplicated(x$fishing_event_id),]
dd <- x[duplicated(x$skate_id),]

dd2 <- filter(x, (fishing_event_id %in% c(dd$fishing_event_id)))


xx <- d1 |> group_by(survey_series_id, survey_series_desc,
                    # major_stat_area_code,
                    # minor_stat_area_code,
                    original_ind) |>
  summarise(n = n())


d2 <- get_all_survey_sets(species = spp, ssid = c(#35, 41, 42,
                                                  43), major = NULL)


d <-get_all_survey_sets(species = spp, ssid = NULL, major = c("03","04"))

xx <- d |> group_by(survey_series_id, survey_series_desc,
                     # major_stat_area_code,
                     # minor_stat_area_code,
                     original_ind) |>
  summarise(n = n())

