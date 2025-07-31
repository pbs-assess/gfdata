# Get commercial catch data for CCIRA analysis May 2024
# (1) Query GFFOS
# (2) Visualise trade-off between data resolution and data privacy
# (3) Save anonymised total catch (count for longline, kg for trawl) as sf objects
#     with grid boundaries specified in the geometry.

library(dplyr)
library(purrr)
library(ggplot2)
library(sf)
library(patchwork)

sf_use_s2(FALSE)

# Need to add MC.LATITUDE and MC.LONGITUDE to get the spatial information from
# GF_MERGED_CATCH (which contains data from historical databases - e.g.,
# PacHarvHL, PacHarvSable, PacHarvest; because GFFOS has data >= 2007)
.q <- "SELECT
  DATABASE_NAME,
  MC.TRIP_ID,
  MC.FISHING_EVENT_ID,
  FISHERY_SECTOR,
  TRIP_CATEGORY,
  GEAR,
  BEST_DATE,
  FE_START_DATE,
  FE_END_DATE,
  LAT,
  LON,
  BEST_DEPTH,
  MC.LATITUDE,
  MC.LONGITUDE,
  MC.SPECIES_CODE,
  MC.DFO_STAT_AREA_CODE,
  MC.DFO_STAT_SUBAREA_CODE,
  SPECIES_SCIENTIFIC_NAME,
  SPECIES_COMMON_NAME,
  LANDED_KG,
  -- CASE WHEN GEAR IN ('TRAP', 'HOOK AND LINE', 'MIDWATER TRAWL') AND YEAR(BEST_DATE) < 2006 THEN 0
	-- WHEN GEAR IN ('BOTTOM TRAWL', 'UNKNOWN TRAWL') AND YEAR(BEST_DATE) <1996 THEN 0
	-- WHEN TRIP_CATEGORY IN ('OPT B') AND YEAR(BEST_DATE) <2006 THEN 0
	-- ELSE DISCARDED_KG END AS DISCARDED_KG,
	DISCARDED_KG,
  LANDED_PCS,
  DISCARDED_PCS,
  MC.MAJOR_STAT_AREA_CODE,
  MC.MINOR_STAT_AREA_CODE,
	MSA.MAJOR_STAT_AREA_NAME,
  VESSEL_NAME,
  VESSEL_REGISTRATION_NUMBER
FROM GFFOS.dbo.GF_MERGED_CATCH MC
	INNER JOIN GFFOS.dbo.SPECIES SP ON SP.SPECIES_CODE = MC.SPECIES_CODE
	INNER JOIN GFBioSQL.dbo.MAJOR_STAT_AREA MSA ON MC.MAJOR_STAT_AREA_CODE = MSA.MAJOR_STAT_AREA_CODE
	LEFT JOIN (SELECT
		TRIP_ID,
    TRIP_CATEGORY,
    FISHING_EVENT_ID,
    LAT,
    LON
	FROM GFFOS.dbo.GF_D_OFFICIAL_FE_CATCH
		GROUP BY TRIP_ID, TRIP_CATEGORY, FISHING_EVENT_ID, LAT, LON) C ON CAST (MC.TRIP_ID AS VARCHAR) = CAST (C.TRIP_ID AS VARCHAR) AND MC.FISHING_EVENT_ID = C.FISHING_EVENT_ID
-- insert species here
ORDER BY BEST_DATE, SPECIES_COMMON_NAME"

q_lines <- readLines(textConnection(.q))

sp_list <- c("467", "418", "437", "435", "405", "401", "451", "414", "407",
  "424", "426", "442", "417", "394", "439", "450", "421", "403",
  "398", "431", "409", "428", "448", "433", "444", "415", "429",
  "423")

# Requries VPN:
# -------------
# Slow, might have been better to iterate over species
.sql <-
  gfdata:::inject_filter("WHERE MC.SPECIES_CODE IN", sp_list,
    sql_code = q_lines, search_flag = "-- insert species here") |>
  paste(collapse = '\n')

catch_spatial <- gfdata::run_sql("GFFOS", .sql)
# saveRDS(catch_spatial, 'catch-spatial.rds')
saveRDS(catch_spatial, "spatial_catch_2025_07_31.rds")

stop("End of current data analysis")

# -------------

# --- Setup coastline ---
coast_utm <- pacea::bc_coast |>
  st_transform(crs = 32609) |>
  st_simplify(dTolerance = 2000) # could simplify more since coast is just for visual anchor

# Get CCIRA polygon
p <- readRDS('ccira_rockfish_bbox.rds') |> st_transform(st_crs(coast_utm))
p_ll <- st_transform(p, 'WGS84')
bbox <- st_bbox(p)
utm_xlim <- c(bbox$xmin, bbox$xmax)
utm_ylim <- c(bbox$ymin, bbox$ymax)

# And crop coast one to this polygon
coast_cropped <- st_crop(coast_utm, bbox)

# Clean the data
# ----------------------
clean_data <- function(commercial_dat) {
  # Use Kyle's year bins
  years <- seq(from = min(commercial_dat$year), to = 2023)
  breaks <- c(-Inf, 2005, 2007, 2012, 2018, 2023)
  labels <- c("<= 2005", "2006 to 2007", "2008 to 2012", "2013 to 2018", "2019 to 2023")
  year_bins <- tibble(
    year = years,
    bin = cut(years, breaks = breaks, labels = labels, right=TRUE, include.lowest=TRUE)
  )

  clean_df <-
    commercial_dat |>
      filter(!is.na(vessel_registration_number)) |>
      left_join(year_bins) |>
      sdmTMB::add_utm_columns(ll_names = c('lon', 'lat'))

   if ('cpue' %in% names(clean_df)) clean_df <- filter(clean_df, !is.na(cpue))
   if ('catch' %in% names(clean_df)) clean_df <- filter(clean_df, !is.na(catch))

  # Make spatial object for simpler plotting
  st_clean_df <- st_as_sf(clean_df, coords = c("lon", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
            st_transform(st_crs(coast_utm))
}
# --------------------

# Enact privacy filter
get_public_grid <- function(dat = st_clean_df, cell_size = 5000,
                            .groups = c('grid_id', 'bin')) {
  group_syms <- syms(.groups)  # can choose to ignore 'bin' group

  if ('cpue' %in% names(dat)) {
    metric <- 'cpue'
  }
  if ('catch' %in% names(dat)) {
    metric <- 'catch'
  }

  # Make grid based on cell size
  pgrid <- p |>
  st_make_grid(cellsize = c(cell_size, cell_size), square = TRUE) |>
  st_sf(geometry = _) |>
  mutate(grid_id = row_number(),
         cell_size = cell_size)

  intersect_df <- st_intersection(dat, pgrid)

  # Count the number of points from each boat within each grid cell
  # counts <- intersect_df |>
  #   group_by(grid_id, cell_size) |>
  #   summarise(count = n(), .groups = "drop") |>
  #   mutate(binning = "all time")
  privacy_by_group <- function(data, group_syms) {
    data |>
      group_by(!!!group_syms) |>
      summarise(count = n(), .groups = "drop") |>
      mutate(privacy_bins := paste(.groups, collapse = "-"))
  }
  counts <- privacy_by_group(intersect_df, group_syms)
  counts_lu <- counts |>
    st_drop_geometry()

  # Get grid cell ids that can be public
  # public_cells <- filter(counts, count >= 3) |>
  #   pull('grid_id')
  public_df <- intersect_df |>
    left_join(counts_lu) |>
    filter(count >= 3) |>
    group_by(!!!group_syms) |>
    #mutate(sum_cpue = sum(cpue)) |>
    mutate(!!metric := ifelse(metric == 'cpue', exp(mean(log(cpue), na.rm = FALSE)),
      sum(catch / 1000, na.rm = FALSE))) |>
    #mutate(cpue = exp(mean(log(cpue), na.rm = FALSE))) |>
    #mutate(catch = sum(catch / 1000, na.rm = FALSE)) |>
    ungroup()

  omitted_df <- intersect_df |>
    left_join(counts_lu) |>
    mutate(omitted = ifelse(count >= 3, 0, 1)) |>
    select(grid_id, bin, count, omitted) |>
    st_drop_geometry() |>
    as_tibble()
  if (nrow(omitted_df) == 0L) {
    omitted_df <- NA
  } else {
    rename_vec <- c(omitted = '1', retained = '0')
    omitted_df <- omitted_df |>
      janitor::tabyl(bin, omitted) |>
      janitor::adorn_totals("row") |>
      janitor::adorn_percentages("row") |>
      janitor::adorn_pct_formatting() |>
      rename(any_of(rename_vec))
  }

  public_grid_df <- public_df |>
    st_drop_geometry() |>
    left_join(pgrid) |>
    st_sf(geometry = _)

  out <- list(pgrid = pgrid, public_grid_df = public_grid_df, omitted_df = omitted_df)
}

plot_public <- function(dat, metric, facet_yearbin = FALSE) {
  # fix axis breaks
  x_breaks <- c(-129, -127)
  y_breaks <- c(51, 53)

  species <- unique(dat$public_grid_df$species_common_name)

  if (length(species) == 0L) {
    title <- paste(unique(dat$pgrid$cell_size) / 1000, "km\n", sep = " ")
    g <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data", hjust = 0.5, vjust = 0.5, size = 5) +
      theme_void() + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
  } else {
    omitted_df <- dat$omitted_df |> slice(1:5)
    total_omit <- pull(dat$omitted_df |> filter(bin == "Total"), 'omitted')
    title <- paste(unique(dat$pgrid$cell_size) / 1000, "km\nomit: ", total_omit, sep = " ")

    g <- ggplot() +
      #geom_sf(data = dat$pgrid, fill = NA, colour = "grey50", stroke = 0.5) + # Makes huge file
      geom_sf(data = coast_cropped, lwd = 0.3, fill = "grey85", colour = "grey72") +
      #geom_sf(data = st_crop(coast_utm, st_bbox(p)), lwd = 0.3, fill = "grey85", colour = "grey72") +
      coord_sf(x = utm_xlim, y = utm_ylim, expand = TRUE, crs = st_crs(coast_utm)) +
      geom_sf(data = dat$public_grid_df, aes(fill = !!sym(metric))) +
      viridis::scale_fill_viridis(trans = "log10") +
      theme_minimal() +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks)

    if (facet_yearbin) g <- g + facet_wrap(~ bin, ncol = 5)

    g <- g + geom_text(data = omitted_df,
      mapping = aes(x = 640000, y = 5900000, label = omitted),
      size = 2)
  }

  title_plot <- ggplot() +
    annotate("text", x = 1, y = 0.5, label = title, hjust = 0.5, vjust = 0.5, size = 4) +
    theme_void() +
    theme(plot.margin = margin(0, 0.3, 0, 0.3, "cm"))

  (title_plot + g) + patchwork::plot_layout(widths = c(0.1, 1))
}

cross_sp_grid_sizes <- function(dat, species, cell_sizes) {
  public_dat <- dat |>
  filter(species_common_name == species) |>
  replicate(length(cell_sizes), expr = _, simplify = FALSE) |>
  purrr::map2(.y = cell_sizes, \(.x, .y)
    get_public_grid(dat = .x, cell_size = .y, .groups = c('grid_id', 'bin'))
  )
}

plot_sp_by_grids <- function(public_dat, species, metric, plot_title) {
  public_plots <- public_dat |>
    map(\(x) plot_public(x, metric, facet_yearbin = TRUE))

  patchwork::wrap_plots(public_plots, nrow = 5) +
  patchwork::plot_annotation(paste(plot_title, species, sep = " - "))
}

# ------------------------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------------------------

dat_list <- readr::read_csv('ccira_data_for_dfo.csv')
sp_list <- unique(dat_list$dfo2)

catch_data <- readRDS('catch-spatial2.rds') |>
  janitor::clean_names() |>
  mutate(year = lubridate::year(best_date)) |>
  filter(year >= 1996, year < 2024) |> # trawl not reliable before 1996
  mutate(species_common_name = tolower(species_common_name)) |>
  mutate(gfbio_lat = lat,
         merged_lat = latitude, # keep these for reference
         lat = ifelse(is.na(lat), latitude, lat),
         gfbio_lon = lon,
         merged_lon = longitude,
         lon = ifelse(is.na(lon), longitude, lon)) |>
  filter(!is.na(lat), !is.na(lon))

trawl_catch_spatial <- catch_data |>
  filter(gear == "BOTTOM TRAWL") |>
  mutate(catch = discarded_kg + landed_kg) |>
  clean_data()

midwater_catch_spatial <- catch_data |>
  filter(gear == "MIDWATER TRAWL") |>
  mutate(catch = discarded_kg + landed_kg) |>
  clean_data()

ll_catch_dat <- catch_data |>
  filter(year >= 2007) |>
  filter(gear %in% c("HOOK AND LINE", "LONGLINE")) |>
  mutate(catch = discarded_pcs + landed_pcs) |>
  clean_data()

cell_sizes <- c(3, 4, 5, 6, 7) * 1000

# ------------------------------------------------------------------------------
# Visualise data data resolution and data privacy trade-off
# ------------------------------------------------------------------------------
# LONGLINE CATCH -----
ll_sp_by_grids <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = ll_catch_dat, cell_sizes = cell_sizes, species = .x))
    }) |>
  setNames(sp_list)
ll_sp_by_grid_plots <- ll_sp_by_grids |>
  imap(\(x, idx) try(plot_sp_by_grids(x, species = idx, metric = "catch",
   plot_title = "Longline Catch (count)")))

pdf("longline-catch.pdf", width = 11, height = 15)
map(ll_sp_by_grid_plots, print)
dev.off()

# BOTTOM TRAWL CATCH -------
trawl_sp_by_grids <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = trawl_catch_spatial, cell_sizes = cell_sizes, species = .x))
    }) |>
  setNames(sp_list)
sp_by_grid_plots <- trawl_sp_by_grids |>
  imap(\(x, idx) try(plot_sp_by_grids(x, species = idx, metric = "catch",
   plot_title = "Trawl Catch (kg)")))

pdf("trawl-catch.pdf", width = 11, height = 15)
map(sp_by_grid_plots, print)
dev.off()

# MIDWATER TRAWL CATCH -----
midwater_sp_by_grids <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = midwater_catch_spatial, cell_sizes = cell_sizes, species = .x))
    }) |>
  setNames(sp_list)
midwater_sp_by_grid_plots <- midwater_sp_by_grids |>
  imap(\(x, idx) try(plot_sp_by_grids(x, species = idx, metric = "catch",
   plot_title = "Midwater Trawl Catch (kg)")))

pdf("midwater-trawl-catch.pdf", width = 11, height = 15)
map(midwater_sp_by_grid_plots, print)
dev.off()

# ------------------------------------------------------------------------------
# Get anonymised data at particular cell size
# ------------------------------------------------------------------------------
cell_sizes <- 4000

ll_4km <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = ll_catch_dat, cell_sizes = cell_sizes, species = .x))
    })

bottom_trawl_4km <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = trawl_catch_spatial, cell_sizes = cell_sizes, species = .x))
    })

midwater_trawl_4km <- sp_list |>
  map(~{message('Species:', .x);
    try(cross_sp_grid_sizes(dat = midwater_catch_spatial, cell_sizes = cell_sizes, species = .x))
    })

# Could optionally add the proportion omitted
ll_out <- ll_4km |>
  map_dfr(\(x) {
    x[[1]]$public_grid_df |>
    group_by(species_common_name, gear, geometry, cell_size, bin) |>
    summarise(total_catch_count = sum(catch))
  })
saveRDS(ll_out, file = 'll_4km_catch.rds')

bottom_trawl_out <- bottom_trawl_4km |>
  map_dfr(\(x) {
    x[[1]]$public_grid_df |>
    group_by(species_common_name, gear, geometry, cell_size, bin) |>
    summarise(total_catch_kg = sum(catch))
  })
saveRDS(bottom_trawl_out, file = 'bottom_trawl_4km_catch.rds')

midwater_trawl_out <- midwater_trawl_4km |>
  map_dfr(\(x) {
    x[[1]]$public_grid_df |>
    group_by(species_common_name, gear, geometry, cell_size, bin) |>
    summarise(total_catch_kg = sum(catch))
  })
saveRDS(midwater_trawl_out, file = 'midwater_trawl_4km_catch.rds')
