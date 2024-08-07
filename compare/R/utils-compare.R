survey <- function (ssid) {
  # Get survey id table
  id_tab <- readRDS(here::here("compare", "data", "ssids.rds"))
  # Return
  id_tab$survey_abbrev[match(ssid, id_tab$ssid)]
}
