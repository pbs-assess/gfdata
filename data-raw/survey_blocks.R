## code to prepare `survey_blocks` dataset goes here

library(sf)
library(ggplot2)
library(dplyr)

# d <- gfdata::get_active_survey_blocks(active_only = FALSE)
# saveRDS(d, "~/Downloads/survey-blocks.rds")
d <- readRDS("~/Downloads/survey-blocks2.rds")

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

survey_blocks <- filter(df2, active_block) |> mutate(depth_m = -depth_m)

attr(survey_blocks, "date-generated") <- Sys.Date()

# stringi::stri_escape_unicode(st_crs(survey_blocks)$wkt)
# st_crs(survey_blocks)$wkt <- gsub("°|º", "\\\u00b0", st_crs(survey_blocks)$wkt)
# stringi::stri_escape_unicode(st_crs(survey_blocks)$wkt)

usethis::use_data(survey_blocks, overwrite = TRUE)
