library(magrittr)

# Spherical geometries problematic
sf::sf_use_s2(FALSE)

# Query GFBio - need DFO VPN access
# pcod_dat <- gfdata::get_survey_sets(species = "pacific cod", ssid = 7)
# pcod_dat <- pcod_dat |>
#   filter(survey_abbrev == 'MSSM WCVI') |>
#   filter(grepl('WCVI Shrimp Survey Area 124|125', grouping_desc))

#saveRDS(pcod_dat, file.path('data-raw', 'pcod-mssm-survey-sets.rds'))
pcod_dat <- readRDS(file.path('data-raw', 'pcod-mssm-survey-sets_2023.rds'))
pcod_dat <- pcod_dat |>
  dplyr::filter(!is.na(longitude)) |>
  sdmTMB::add_utm_columns(c('longitude', 'latitude')) |>
  dplyr::mutate(row_id = dplyr::row_number())

# Use equal distance projection
pcod_sf <-
  pcod_dat |>
  dplyr::select(year, longitude, latitude) |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'WGS84') |>
  sf::st_transform(crs = 32609)
  # dplyr::select(year, X, Y) |>
  # sf::st_as_sf(coords = c('X', 'Y'), crs = 32609)

pcod_years <- dplyr::select(pcod_dat, row_id, year)

# Use 2x2 km grid size in units of our polygon shape file
grid_spacing <- 2000

# Create grid over the bounding box of the polygon
full_grid <- pcod_sf |>
  sf::st_make_grid(cellsize = c(grid_spacing, grid_spacing)) |>
  sf::st_as_sf() |>
  dplyr::rename(geometry = x)

# Get grid cells that overlap with at least one sampling point
intersected <- sf::st_intersects(full_grid, pcod_sf)

id_intersect <- intersected |> purrr::map_dbl(length) > 0
last_year <- intersected |>
  purrr::map_dbl(\(row) if (length(row) > 0) {max(pcod_years[row, ]$year)} else {NA})

full_grid$last_samp_year <- last_year

mssm_grid_sf <- full_grid[id_intersect, ] |>
  dplyr::mutate(survey = "MSSM WCVI")

mssm_grid <- mssm_grid_sf |>
  sf::st_centroid() %>%
  dplyr::mutate(survey = "MSSM WCVI",
                #ssid = 7,
                X = sf::st_coordinates(.)[,1] / 1000, # match sdmTMB coordinate system
                Y = sf::st_coordinates(.)[,2] / 1000, # match sdmTMB coordinate system
                area = grid_spacing / 1000 * grid_spacing / 1000) |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
  dplyr::select(survey, X, Y, area, last_samp_year)

mssm_grid_sf <- mssm_grid_sf |>
  sf::st_transform(crs = "WGS84")

# Option to save an SF object
#saveRDS(mssm_grid_sf, file.path('data-raw', 'mssm_grid_sf.rds'))
#usethis::use_data(mssm_grid)
# Visualise grid before and after 2009
# mssm_grid_sf |>
#   mutate(last_samp_year = ifelse(last_samp_year >= 2009, ">= 2009", " < 2009")) |>
# ggplot(data = _) +
#   geom_sf(aes(fill = last_samp_year), alpha = 0.8) +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   gfplot::theme_pbs()

usethis::use_data(mssm_grid, overwrite = TRUE)
usethis::use_data(mssm_grid_sf, overwrite = TRUE, internal = TRUE)
