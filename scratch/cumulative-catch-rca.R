library(sf)
sf_use_s2(FALSE)

library(ggplot2)
library(dplyr)
library(future)
options(future.globals.maxSize = 2000 * 1024^2)
plan(multicore, workers = 8L)
options(future.rng.onMisuse = "ignore")

setwd(here::here("scratch"))

d <- readRDS("ccira_sdmTMB_data_locations.rds")
dat_rca_cpue <- select(d, species, date, longitude, latitude, rca_establishment) |>
  as_tibble() |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ) |>
  st_transform(st_crs(32609))
dat_rca_cpue$date <- lubridate::ymd(dat_rca_cpue$date)

coast_utm <- pacea::bc_coast |>
  st_transform(crs = 32609) |>
  st_simplify(dTolerance = 2000)

ggplot(dat_rca_cpue) +
  geom_sf() +
  geom_sf(data = coast_utm)

catch <- readRDS("catch-spatial.rds")

weights <- readr::read_csv("species_weights_1.csv") |> select(-1)
weights$species_dfo <- ifelse(!is.na(weights$species_dfo), weights$species_dfo, paste(weights$species, "rockfish"))
dat_rca_cpue <- left_join(dat_rca_cpue, weights)

catch_data <- catch |>
  janitor::clean_names() |>
  mutate(year = lubridate::year(best_date)) |>
  filter(year >= 1996, year < 2024) |>
  mutate(species_common_name = tolower(species_common_name)) |>
  mutate(
    gfbio_lat = lat,
    merged_lat = latitude,
    lat = ifelse(is.na(lat), latitude, lat),
    gfbio_lon = lon,
    merged_lon = longitude,
    lon = ifelse(is.na(lon), longitude, lon)
  ) |>
  filter(!is.na(lat), !is.na(lon)) |>
  as_tibble() |>
  filter(lon > -130, lon < -122, lat > 50, lat < 54) |>
  filter(!is.na(vessel_registration_number)) |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ) |>
  st_transform(st_crs(32609)) |>
  filter(species_common_name %in% unique(dat_rca_cpue$species_dfo))

catch_data <- left_join(catch_data, rename(weights, species_common_name = species_dfo))

filter(catch_data, is.na(median_weight)) |>
  pull(species_common_name) |>
  table()

trawl_catch <- catch_data |>
  filter(gear == "BOTTOM TRAWL") |>
  mutate(catch = discarded_kg + landed_kg)

midwater_catch <- catch_data |>
  filter(gear == "MIDWATER TRAWL") |>
  mutate(catch = discarded_kg + landed_kg)

ll_catch <- catch_data |>
  # filter(year >= 2006) |>
  filter(gear %in% c("HOOK AND LINE", "LONGLINE")) |>
  mutate(catch = (discarded_pcs + landed_pcs) * median_weight)

# ll_catch |>
#   filter(species_common_name == "yelloweye rockfish") |>
#   ggplot() +
#   geom_sf(data = coast_utm) + facet_wrap(~year) +
#   geom_sf(pch = 21, size = 0.2)

combined_catch <- bind_rows(trawl_catch, midwater_catch, ll_catch) |>
  filter(best_date < max(dat_rca_cpue$date)) |>
  select(year, best_date, species_common_name, catch, gear)

find_catch_within_distance <- function(dat_rca_cpue_row, input_catch_data, distance = 1000) {
  # sum catch within X m of each point

  cat(dat_rca_cpue_row, "\n")

  sf1 <- dat_rca_cpue[dat_rca_cpue_row, , drop = FALSE]
  sf2 <- filter(
    input_catch_data, species_common_name == sf1$species_dfo,
    best_date < sf1$date
  )

  if (nrow(sf2) == 0L) {
    sf1$cumulative_catch <- 0
    return(sf1)
  }

  # ggplot(sf1) + geom_sf(data = coast_utm) + geom_sf() +
  #   geom_sf(data = sf2, colour = "red")

  sf1$id <- 1:nrow(sf1)
  sf2$obs_id <- 1:nrow(sf2)

  within_buffer <- st_is_within_distance(sf1, sf2, dist = distance)

  match_df <- data.frame(
    id = rep(sf1$id, lengths(within_buffer)),
    obs_id = sf2$obs_id[unlist(within_buffer)]
  )
  if (nrow(match_df) == 0L) {
    sf1$cumulative_catch <- 0
    return(sf1)
  }

  summary_df <- match_df |>
    left_join(sf2, by = "obs_id") |>
    mutate(rca_establishment = sf1$rca_establishment[[1]]) |>
    mutate(before_rca = year < rca_establishment) |>
    group_by(id, before_rca) %>%
    summarise(cumulative_catch = sum(catch, na.rm = TRUE), .groups = "drop")

  left_join(sf1, summary_df, by = "id")
}

# find_catch_within_distance(2, combined_catch) |> glimpse()
# find_catch_within_distance(11, combined_catch) |> glimpse()

tictoc::tic()
# out <- purrr::map_dfr(1:1000, \(i) find_catch_within_distance(i, combined_catch))
# out <- furrr::future_map_dfr(1:1000, \(i) find_catch_within_distance(i, combined_catch))
out <- furrr::future_map_dfr(1:nrow(dat_rca_cpue), \(i) find_catch_within_distance(i, combined_catch))
tictoc::toc()

saveRDS(out, file = "cumulative-catch-all.rds")
# saveRDS(out, file = "cumulative-catch-2006.rds")

out <- readRDS("cumulative-catch-all.rds")
# out <- readRDS("cumulative-catch-2006.rds")

ggplot(out) +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c() +
  scale_size_area() +
  facet_wrap(~species)

table(out$before_rca)

out |>
  filter(before_rca) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c() +
  scale_size_area() +
  facet_wrap(~species)

out |>
  filter(!before_rca) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c() +
  scale_size_area() +
  facet_wrap(~species)

setwd(here::here())
