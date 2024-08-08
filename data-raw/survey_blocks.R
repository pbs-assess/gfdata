## code to prepare `survey_blocks` dataset goes here

library(sf)
library(ggplot2)
library(dplyr)

# d <- gfdata::get_active_survey_blocks(active_only = FALSE)
# saveRDS(d, "~/Downloads/survey-blocks.rds")
d <- readRDS("~/Downloads/survey-blocks.rds")

download_date <- lubridate::date(file.info("~/Downloads/survey-blocks.rds")$ctime)

d$id <- seq_len(nrow(d))
polys <- split(d, d$id) |> lapply(\(x) {
  list(rbind(
    c(x$pt1_lon, x$pt1_lat),
    c(x$pt2_lon, x$pt2_lat),
    c(x$pt3_lon, x$pt3_lat),
    c(x$pt4_lon, x$pt4_lat),
    c(x$pt1_lon, x$pt1_lat)
  )) |> st_polygon()
})
dd <- select(d, survey_series_name, survey_series_id, block_id = block_designation, depth_m = survey_site_depth_m, active_block = selection_ind)
dd$geometry <- st_sfc(polys)
df <- st_as_sf(dd)
st_crs(df) <- 4326
df <- st_transform(df, crs = 32609)

df |> filter(active_block, !is.na(survey_series_name)) |>
  ggplot(aes(colour = survey_series_name)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")

df |> filter(!active_block, !is.na(survey_series_name)) |>
  ggplot(aes(colour = survey_series_name)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")

df |> filter(!is.na(survey_series_name)) |>
  ggplot(aes(colour = depth_m)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_viridis_c()

df |> filter(active_block, !is.na(survey_series_name)) |>
  ggplot(aes(colour = depth_m)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_viridis_c()


lu <- structure(list(survey_abbrev = c("SYN QCS", "HS MSA", "SYN HS",
  "SYN WCVI", "IPHC FISS", "HBLL OUT N", "SYN WCHG", "HBLL OUT S",
  "HBLL INS N", "HBLL INS S", "MSSM WCVI"), survey_series_id = c(1,
    2, 3, 4, 14, 22, 16, 36, 39, 40, 7)), row.names = c(NA, -11L), class = c("data.frame"))

df2 <- left_join(df, lu)

df2 <- filter(df2, !survey_abbrev %in% c("IPHC FISS"))

df2 |> filter(active_block) |>
  ggplot(aes(colour = survey_abbrev)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")

df2 <- select(df2, survey_abbrev, survey_series_id, block_id, depth_m, active_block, geometry)

df2 |> filter(active_block) |>
  ggplot(aes(colour = survey_abbrev)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")

# usethis::use_data_raw("survey_blocks")
# mapview::mapview(df)

# Get overwater area for HBLL grids
# ---------------------------------
# The documentation of this coastline shapefile could be better,
# But at least this is a polygon rather than the freshwater atlas and the CHS coastline
# https://open.canada.ca/data/dataset/30aeb5c1-4285-46c8-b60b-15b1a6f4258b
# https://catalogue.data.gov.bc.ca/dataset/province-of-british-columbia-boundary-terrestrial
# "Province of British Columbia - Boundary Terrestrial  PUBLISHED
# Published By GeoBC Branch
# Description
# This dataset consists of polyline, single part, and multi part polygons representing the Province of British Columbia.
# The terrestrial portion of the boundary was derived from the Administrative Boundaries Management System (ABMS) representation of the province: Province of British Columbia - Legally Defined Administrative Areas of BC.
# The coastal portion of the boundary differs from the ABMS boundary and was derived from the Freshwater Atlas (FWA): Freshwater Atlas Coastlines.
# This boundary may be updated periodically, as more accurate data becomes available.
# Due to the structure of the data, it does not meet the technical requirements to be published in the BC Geographic Warehouse (BCGW). It is available for download as an OGC GeoPackage and as an ESRI File Geodatabase in the Data and Resources box to the the right."
# ---
st_layers("~/Downloads/BC_Boundary_Terrestrial.gpkg") # get layer name of single polygon
bc_poly <- st_read("~/Downloads/BC_Boundary_Terrestrial.gpkg", layer = "BC_Boundary_Terrestrial_Multipart")

# HBLL INS ----
llin <- df2 |> filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"))
llin_bbox <- st_bbox(llin)
ll_crs <- st_crs(llin)

small_bc_ins <- bc_poly |>
  st_transform(ll_crs) |>
  st_crop(llin_bbox)

overlap_ins <- st_intersection(small_bc_ins, llin)
llin_ow <- st_difference(llin, small_bc_ins) %>%
  mutate(area = st_area(., units = "km^2"))

# Looks pretty good
ggplot() +
  geom_sf(data = llin, fill = "blue") +
  geom_sf(data = small_bc_ins) +
  geom_sf(data = overlap_ins, fill = "red") +
  geom_sf(data = llin_ow, fill = "black")

# Compare with old overwater df:
overwater_df <- readRDS('~/R_DFO/gfsynopsis/report/data-cache-2024-05/grids/hbll_ins_grid.rds')
odf2 <- overwater_df |>
  mutate(X = X * 1000, Y = Y * 1000) |>
  st_as_sf(coords = c('X', 'Y'), crs = st_crs(llin))

compare_df <- st_join(llin_ow, odf2, suffix = c("_new", "_old")) |>
  mutate(area_old = units::set_units(area_old, km^2)) |>
  mutate(area_diff = area_new - area_old) |>
  units::drop_units()

# There are a few big differences, but without out knowing exactly how the first
# file was calculated it's unclear if we can know why.
hist(compare_df$area_diff)

# See if anything pops out from a visual inspection
p1 <-
compare_df |>
  arrange(-abs(area_diff)) |>
  sf::st_cast("MULTIPOLYGON") |>
ggplot() +
  geom_sf(data = small_bc_ins) +
  geom_sf(aes(fill = area_diff)) +
  geom_sf(data = odf2) +
  geom_sf(data = pacea::bc_coast |> # lower resolution than BC Boundary shapefile (pacea uses the rnaturalearth::ne_coastline(scale = 10))
    st_transform(ll_crs) |>
    st_crop(llin_bbox), fill = NA, colour = "blue") +
  viridis::scale_fill_viridis()
p1

#plotly::ggplotly(p1) # easier to zoom in as needed

# HBLL OUT ----
llout <- df2 |> filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"))
llout_bbox <- st_bbox(llout)

small_bc_out <- bc_poly |>
  st_transform(ll_crs) |>
  st_crop(llout_bbox)

overlap_out <- st_intersection(small_bc_out, llout)
llout_ow <- st_difference(llout, small_bc_out) %>%
  mutate(area = st_area(., units = "km^2"))

# Most cells are completely in the water, but a few seem to overlap with land
hist(llout_ow$area)

p2 <-
ggplot(llout) +
  geom_sf(fill = "blue") +
  geom_sf(data = small_bc_out) +
  geom_sf(data = overlap_out, fill = "red") +
  geom_sf(data = llout_ow, fill = "black")
# plotly::ggplotly(p2)
# ----

# Get area for all survey blocks
# ---------------------------------
llin_ow_simple <- select(llin_ow, names(df2))
llout_ow_simple <- select(llout_ow, names(df2))

# Calculate area on highest resolution polygons
area_df <- df2 |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  bind_rows(llin_ow_simple, llout_ow_simple) %>%
  mutate(area = units::set_units(st_area(.), "km^2")) |> # being explicit with units to reduce mistakes
  units::drop_units() |>
  select(survey_abbrev, block_id, area) |>
  st_drop_geometry()

# Join back with the original grid polygons
survey_blocks <- df2 |>
  left_join(area_df) |>
  mutate(depth_m = -depth_m)

# This is an option to save a data object that keeps the coastline-intersected polygon
# survey_blocks <- df2 |>
#   filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
#   bind_rows(llin_ow_simple, llout_ow_simple) %>%
#   mutate(depth_m = -depth_m,
#          area = units::set_units(st_area(.), "km^2")) |> # being explicit with units to reduce mistakes
#   units::drop_units()
# After calculating area, could make the data object smaller for storage in the package
# Size before getting overwater area: 528K	data/survey_blocks.rda
# Not sure how small to make this, pretty wonky at dTolerance = 500.
# # survey_blocks <- st_simplify(survey_blocks, preserveTopology = TRUE, dTolerance = 200) # option to make it smaller
# Look at difference in resolution compared to coastline
# p <-
# ggplot() +
#   geom_sf(data = small_bc_ins) +
#   geom_sf(data = survey_blocks |>
#     filter(stringr::str_detect(survey_abbrev, "HBLL INS")) |>
#     sf::st_cast("MULTIPOLYGON"), # needed for using plotly
#     fill = "blue") #+
# plotly::ggplotly(p)

# This is still ending up as a bigger object than I was expecting just by adding area?
utils:::format.object_size(file.size("data/survey_blocks.rda"), units = "Mb", digits = 2)

attr(survey_blocks, "date-downloaded") <- download_date
attr(survey_blocks, "date-generated") <- Sys.Date()

# stringi::stri_escape_unicode(st_crs(survey_blocks)$wkt)
# st_crs(survey_blocks)$wkt <- gsub("°|º", "\\\u00b0", st_crs(survey_blocks)$wkt)
# stringi::stri_escape_unicode(st_crs(survey_blocks)$wkt)

usethis::use_data(survey_blocks, overwrite = TRUE)
