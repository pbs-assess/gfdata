library(sf)
sf_use_s2(FALSE)

library(ggplot2)
library(dplyr)
library(future)
options(future.globals.maxSize = 2000 * 1024^2)
plan(multicore, workers = 5L)
options(future.rng.onMisuse = "ignore")

setwd(here::here("scratch"))

# talk to Maria...
# before 2006 OK?
# concatenate survey_id and species into an ID to join on

d <- readRDS("ccira_sdmTMB_data_locations.rds")
dat_rca_cpue <- select(d, species, date, longitude, latitude, rca_establishment) |>
  as_tibble() |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ) |>
  st_transform(st_crs(32609))
dat_rca_cpue$date <- lubridate::ymd(dat_rca_cpue$date)
dat_rca_cpue$ccira_row_id <- 1:nrow(dat_rca_cpue)

coast_utm <- pacea::bc_coast |>
  st_transform(crs = 32609) |>
  st_simplify(dTolerance = 2000)

ggplot(dat_rca_cpue) +
  geom_sf() +
  geom_sf(data = coast_utm)

catch_raw <- readRDS("spatial_catch_2025_07_31.rds")

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

test <- filter(catch_data, is.na(median_weight)) |>
  pull(species_common_name) |>
  table()
stopifnot(nrow(test) == 0L)

table(catch_data$gear)
table(catch_data$gear, catch_data$year)

theme_set(theme_light())

line_year <- 2006
g1 <- catch_data |> 
  group_by(year, gear) |>  
  summarize(discarded_kg = sum(discarded_kg)) |> 
  ggplot(aes(year, discarded_kg)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~gear, scales = "free_y")

g2 <- catch_data |> 
  group_by(year, gear) |>  
  summarise(discarded_pcs = sum(discarded_pcs)) |> 
  ggplot(aes(year, discarded_pcs)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~gear, scales = "free_y")

cowplot::plot_grid(g1, g2, ncol = 1)
ggsave("discarded-by-gear.pdf", width = 9, height = 9)

g1 <- catch_data |> 
  group_by(year, gear) |>  
  summarize(landed_kg = sum(landed_kg)) |> 
  ggplot(aes(year, landed_kg)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~gear, scales = "free_y")

g2 <- catch_data |> 
  group_by(year, gear) |>  
  summarize(landed_pcs = sum(landed_pcs)) |> 
  ggplot(aes(year, landed_pcs)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~gear, scales = "free_y")

cowplot::plot_grid(g1, g2, ncol = 1)
ggsave("landed-by-gear.pdf", width = 9, height = 9)

g1 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |>
  summarize(landed_kg = sum(landed_kg)) |> 
  ggplot(aes(year, landed_kg)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")

g2 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |> 
  summarize(landed_pcs = sum(landed_pcs)) |> 
  ggplot(aes(year, landed_pcs)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")

cowplot::plot_grid(g1, g2, ncol = 1)
ggsave("landed-by-sp.pdf", width = 10, height = 10)

g1 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |> 
  summarize(discarded_kg = sum(discarded_kg)) |> 
  ggplot(aes(year, discarded_kg)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")

g2 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |>
  summarize(discarded_pcs = sum(discarded_pcs)) |> 
  ggplot(aes(year, discarded_pcs)) + geom_col() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")

cowplot::plot_grid(g1, g2, ncol = 1)
ggsave("discarded-by-sp.pdf", width = 10, height = 10)

g1 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |>
  summarize(discarded_kg_per_pc = sum(discarded_kg)/sum(discarded_pcs)) |> 
  ggplot(aes(year, discarded_kg_per_pc)) + geom_line() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")
g1
ggsave("discarded-kg-per-pc.pdf", width = 10, height = 5)

g1 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP")) |> 
  group_by(year, species_common_name) |>
  summarize(landed_kg_per_pc = sum(landed_kg)/sum(landed_pcs)) |> 
  ggplot(aes(year, landed_kg_per_pc)) + geom_line() +
  geom_vline(xintercept = c(line_year), lty = 2) +
  facet_wrap(~species_common_name, scales = "free_y")
g1
ggsave("landed-kg-per-pc.pdf", width = 10, height = 5)

g1 <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP"), year >= 2006) |> 
  group_by(species_common_name) |>
  # summarize(landed_kg_per_pc = sum(landed_kg)/sum(landed_pcs), median_weight_input = mean(median_weight)) |> 
  summarize(landed_kg_per_pc = median(landed_kg/landed_pcs, na.rm = TRUE), median_weight_input = mean(median_weight)) |> 
  ggplot(aes(landed_kg_per_pc, median_weight_input)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = species_common_name),  max.overlaps = 100) +
  scale_y_log10() + scale_x_log10() +
  coord_equal(ylim = c(0.1, 25), xlim = c(0.1, 25))
g1
ggsave("hook-and-line-trap-weights.pdf", width = 7, height = 7)

new_weights <- catch_data |> 
  filter(gear %in% c("HOOK AND LINE", "TRAP"), year >= 2006) |> 
  group_by(species_common_name) |>
  summarize(median_weight = median(landed_kg/landed_pcs, na.rm = TRUE)) |>
  as.data.frame() |> select(species_common_name, median_weight)
new_weights

catch_data$median_weight <- NULL
catch_data <- left_join(catch_data, new_weights)

trawl_catch <- catch_data |>
  filter(gear %in% c("BOTTOM TRAWL", "UNKNOWN TRAWL", "MIDWATER TRAWL")) |>
  mutate(catch = discarded_kg + landed_kg)

ll_catch <- catch_data |>
  filter(gear %in% c("HOOK AND LINE", "LONGLINE", "TRAP")) |>
  mutate(
    catch = ifelse(
      year < 2006, 
      discarded_kg + landed_kg, 
      discarded_pcs * median_weight + landed_kg
    )
  )

ll_catch |>
# midwater_catch |>
  filter(species_common_name == "yelloweye rockfish") |>
  # filter(species_common_name == "widow rockfish") |>
  # filter(year < 2006) |>
  # filter(landed_kg > 0) |>
  # filter(landed_pcs > 0) |>
  # filter(discarded_pcs > 0) |>
  # filter(discarded_kg > 0) |>
  ggplot() +
  # geom_sf(data = coast_utm) +
  facet_wrap(~year) +
  geom_sf(pch = 21, mapping = aes(size = catch, colour = log(catch))) +
  scale_size_area() +
  scale_colour_viridis_c()

combined_catch <- bind_rows(trawl_catch, ll_catch) |>
  filter(best_date < max(dat_rca_cpue$date)) |>
  select(year, best_date, species_common_name, catch, gear, vessel_registration_number) |>
  filter(catch > 0)

get_radius <- function(area) {
  sqrt(area / pi)
}
get_radius(10)
pi * 2^2

find_catch_within_distance <- function(dat_rca_cpue_row, input_catch_data, distance = 1500) {
  cat(dat_rca_cpue_row, "\n")

  sf1 <- dat_rca_cpue[dat_rca_cpue_row, , drop = FALSE]
  sf2 <- filter(
    input_catch_data, species_common_name == sf1$species_dfo,
    best_date < sf1$date
  )

  if (nrow(sf2) == 0L) {
    sf1$cumulative_catch <- 0
    sf1$nvessel <- 0
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
    sf1$nvessel <- 0
    return(sf1)
  }

  summary_df <- match_df |>
    left_join(sf2, by = "obs_id") |>
    filter(catch > 0) |>
    mutate(rca_establishment = sf1$rca_establishment[[1]]) |>
    mutate(before_rca = year < rca_establishment) |>
    group_by(id) |>
    mutate(
      nvessel = length(unique(vessel_registration_number)),
      .groups = "drop"
    ) |>
    group_by(id, before_rca) |>
    summarise(
      cumulative_catch = sum(catch, na.rm = TRUE),
      nvessel = nvessel[1],
      .groups = "drop"
    )
  if (nrow(summary_df) == 0L) {
    sf1$cumulative_catch <- 0
    sf1$nvessel <- 0
    return(sf1)
  }

  left_join(sf1, summary_df, by = "id")
}

glimpse(dat_rca_cpue)
dat_rca_cpue$median_weight <- NULL
find_catch_within_distance(1, combined_catch) |> glimpse()
find_catch_within_distance(2, combined_catch) |> glimpse()
# find_catch_within_distance(11, combined_catch) |> glimpse()

tictoc::tic()
# out <- purrr::map_dfr(1:10, \(i) find_catch_within_distance(i, combined_catch))
# out <- furrr::future_map_dfr(1:1000, \(i) find_catch_within_distance(i, combined_catch))
out <- furrr::future_map_dfr(1:nrow(dat_rca_cpue), \(i) find_catch_within_distance(i, combined_catch))
tictoc::toc()

out |>
  filter(cumulative_catch > 0) |>
  filter(is.na(before_rca))

# saveRDS(out, file = "cumulative-catch-all-1km.rds")
saveRDS(out, file = "cumulative-catch-all-1.5km.rds")
# saveRDS(out, file = "cumulative-catch-all-2km.rds")
# saveRDS(out, file = "cumulative-catch-2006.rds")

# out <- readRDS("cumulative-catch-all-2km.rds")
out <- readRDS("cumulative-catch-all-1km.rds")
out |> filter(nvessel < 3) |> saveRDS("cumulative-catch-all-1km-nvessel.rds")


# "black"
# "2007-05-15
# median weight== 1.3987
filter(out, species == "black", date == lubridate::ymd("2007-05-15"), round(median_weight, 4) == 1.3987)



out <- readRDS("cumulative-catch-all-1.5km.rds")
out |> filter(nvessel < 3) |> saveRDS("cumulative-catch-all-1.5km-nvessel.rds")

ndisc <- out |>
  filter(nvessel < 3) |>
  nrow()

nretain <- out |>
  filter(nvessel >= 3) |>
  nrow()

ndisc
nretain
nretain / (ndisc + nretain), 2) * 100

check <- out |>
  as.data.frame() |>
  mutate(three_or_more = nvessel >= 3) |>
  group_by(three_or_more) |>
  summarise(cumulative_catch = sum(cumulative_catch))

include <- filter(check, three_or_more) |> pull(cumulative_catch)
exclude <- filter(check, !three_or_more) |> pull(cumulative_catch)
round(include / (include + exclude), 2) * 100

out |>
  filter(cumulative_catch > 0) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = nvessel, size = nvessel)) +
  scale_colour_viridis_c(trans = "log10") +
  scale_size_area() +
  facet_wrap(~species)

out |>
  filter(cumulative_catch > 0) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c(trans = "log10") +
  scale_size_area() +
  facet_wrap(~species)

table(out$before_rca)

out |>
  filter(before_rca) |>
  filter(cumulative_catch > 0) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c(trans = "log10") +
  scale_size_area() +
  facet_wrap(~species)

out |>
  filter(cumulative_catch > 0) |>
  filter(!before_rca) |>
  ggplot() +
  geom_sf(pch = 21, mapping = aes(colour = cumulative_catch, size = cumulative_catch)) +
  scale_colour_viridis_c(trans = "log10") +
  scale_size_area() +
  facet_wrap(~species)


setwd(here::here())
