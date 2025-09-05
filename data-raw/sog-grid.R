library(sf)
library(ggplot2)
library(dplyr)
devtools::load_all("../gfplot")

sog_ye <- readRDS("~/Downloads/sog-ye.rds")

sog <- gfdata::get_active_survey_blocks(ssid = 45, active_only = FALSE)
sog <- readRDS("~/Downloads/sog-blocks.rds")

download_date <- lubridate::date(file.info("~/Downloads/sog-blocks.rds")$ctime)

sog_sf <- sog |>
  filter(selection_ind == TRUE) |>
  gfdata::sql_geom_to_sf() |>
  st_transform(crs = 32609) |>
  mutate(survey_abbrev = "SYN SOG") |>
  select(
    survey_series_id,
    survey_series_name,
    block_id = block_designation,
    grouping_code, # used to join with strata GROUPING data
    depth_m = survey_site_depth_m,
    active_block = selection_ind)

# st_layers("~/Downloads/BC_Boundary_Terrestrial.gpkg") # get layer name of single polygon
bc_poly <- st_read("~/Downloads/BC_Boundary_Terrestrial.gpkg", layer = "BC_Boundary_Terrestrial_Multipart")

# Calculate overwater area
sog_bbox <- st_bbox(sog_sf)
sog_crs <- st_crs(sog_sf)

small_bc_sog <- bc_poly |>
  st_transform(sog_crs) |>
  st_crop(sog_bbox)

overlap_sog <- st_intersection(small_bc_sog, sog_sf)
sog_ow <- st_difference(sog_sf, small_bc_sog) %>%
  mutate(area = st_area(.)) |>
  mutate(area = units::set_units(area, km^2))

# Looks pretty good
ggplot() +
  geom_sf(data = small_bc_sog) +
  geom_sf(data = overlap_sog, fill = "red") +
  geom_sf(data = sog_ow, fill = "black")

sog_grid <- sog_ow |>
  mutate(depth_m = -depth_m,
         survey_abbrev = "SYN SOG") |>
  select(
    survey_series_id,
    survey_abbrev,
    block_id,
    grouping_code,
    depth_m,
    active_block,
    area
  )

utils:::format.object_size(file.size("data/sog_grid.rda"), units = "Mb", digits = 2)

attr(sog_grid, "date-downloaded") <- download_date
attr(sog_grid, "date-generated") <- Sys.Date()

# stringi::stri_escape_unicode(st_crs(sog_grid)$wkt)
# st_crs(sog_grid)$wkt <- gsub("°|º", "\\\u00b0", st_crs(sog_grid)$wkt)
# stringi::stri_escape_unicode(st_crs(sog_grid)$wkt)

usethis::use_data(sog_grid, overwrite = TRUE)
