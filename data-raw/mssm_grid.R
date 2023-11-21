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

pcod_years <- dplyr::select(pcod_dat, row_id, year)

# Use 3x3 km grid size in units of our polygon shape file
grid_spacing <- 3000

# Create grid over the bounding box of the polygon
full_grid <- pcod_sf |>
  sf::st_make_grid(cellsize = c(grid_spacing, grid_spacing)) |>
  sf::st_as_sf() |>
  dplyr::rename(geometry = x)

# Get grid cells that overlap with at least one sampling point
intersected <- sf::st_intersects(full_grid, pcod_sf)

id_intersect <- intersected |> purrr::map_dbl(length) > 0

sampling_years <- intersected |>
      purrr::map(\(row) if (length(row) > 0) {pcod_years[row, ]$year} else {NA})

mssm_grid_sf <- full_grid |>
    dplyr::mutate(year = sampling_years) |>
    tidyr::unnest(cols = year) |>
    dplyr::filter(!is.na(year)) |>
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
  dplyr::select(survey, X, Y, area, year)

mssm_grid_sf <- mssm_grid_sf |>
  sf::st_transform(crs = "WGS84")

# Check grid. Visualise grid for 2009 - 2021.
# Also highlight cells sampled in 2021 and overlay with actual sampling locations in 2021.
# mssm_grid_sf |>
#   dplyr::filter(year >= 2009 & year <= 2021) |>
#   dplyr::distinct(geometry, .keep_all = TRUE) |>
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(ggplot2::aes(fill = year)) +
#   ggplot2::geom_sf(data = mssm_grid_sf |> dplyr::filter(year == 2021), fill = 'pink') +
#   ggplot2::geom_sf(data = pcod_sf |> dplyr::filter(year == 2021), shape = 21, size = 3, fill = 'white')

usethis::use_data(mssm_grid, overwrite = TRUE)
usethis::use_data(mssm_grid_sf, overwrite = TRUE, internal = TRUE)
