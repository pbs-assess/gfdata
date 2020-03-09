# looking at ratios of rex sole to cocaught species
# that were recorded well historically. want to estimate historic discards from
# modern ratio of discards to catch of other species.
# 1. pull catch for target species and primary cocaught species (from get_cocaught_species).
# 2. sum annual catch of target and cocaught landings and discards
# 3. calculate annual ratio of target discard to cocaught landings


# arrange_catch <- function(species){
#   get_catch(species) %>%
#   filter(landed_kg > 0 | discarded_kg >0) %>%
#   select(year, trip_id, fishing_event_id, fishery_sector, gear, landed_kg, discarded_kg) %>%
#   group_by(year, fishery_sector) %>%
#   summarise(landed_kg = sum(landed_kg), discarded_kg = sum(discarded_kg)) %>%
#     ungroup()
#   # summing here (before the join) because need ratio of target to cocaught from ALL records, # not just cocaught records
# }

# #' @export
# #' @param species1
# #' @param species2
# #' @param start_year
# #' @param end_year
# #' @param sector
# #'
# #' @examples
# #' \dontrun{
# #' rex_pcod <- cocaught_ratios(610, 222)
# #' }
# cocaught_ratios <- function(species1, species2, start_year = 1996,
#   end_year = 2006, sector = "GROUNDFISH TRAWL"){
#
#   target_catch <- arrange_catch(species1) %>%
#     rename(target_landed = landed_kg, target_discard = discarded_kg)
#   cocaught_catch <- arrange_catch(species2) %>%
#     rename(cocaught_landed = landed_kg, cocaught_discard = discarded_kg)
#
#   d <- inner_join(target_catch, cocaught_catch, by = c("year", "fishery_sector")) %>%
#     filter(between(year, start_year, end_year), fishery_sector == toupper(sector)) %>%
#     mutate(target_landed/cocaught_landed, target_discard/cocaught_landed)
# }
