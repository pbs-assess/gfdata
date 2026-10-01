#' Load survey block data (polygon, point, or coordinate table)
#'
#' Returns built-in grid datasets as either polygons, centroids, or coordinates.
#' Available datasets include the Synoptic and HBLL survey grids (2x2 km square grids),
#' MSSM grid, and SYN SOG grid.
#' Note that centroid and coordinate outputs may fall on land rather than in the ocean.
#' While suitable for visualization and basic modeling, these points should not be used
#' directly for extracting oceanographic covariates - instead use `polygon` and extract
#' as appropriate.
#'
#' @param dataset Character string specifying the dataset to load. One of:
#'   - `"syn_hbll"` (default): Synoptic and HBLL survey grids combined (2x2 km square grids that may overlap with land).
#'   - `"synoptic"`: all four Synoptic survey grids combined (SYN HS, SYN QCS, SYN WCHG, SYN WCVI).
#'   - `"hbll"`: all four HBLL survey grids combined (HBLL INS N/S, HBLL OUT N/S).
#'   - `"hbll_ins"`: HBLL INS survey grids only (HBLL INS N and HBLL INS S).
#'   - `"hbll_out"`: HBLL OUT survey grids only (HBLL OUT N and HBLL OUT S).
#'   - `"mssm"`: MSSM survey grid data, see `gfdata::mssm_grid`.
#'   - `"syn_sog"`: Strait of Georgia Synoptic Bottom Trawl grid (only active blocks available).
#' @param type Character string specifying the output format. One of:
#'   - `"polygon"` (default): returns an `sf` object with polygon geometries.
#'   - `"centroid"`: returns an `sf` object with the centroid point for each block.
#'   - `"XY"`: returns a `tibble` with columns `X` and `Y` (in kilometres, UTM zone 9N / EPSG:32609),
#'     representing point-on-surface coordinates extracted from each polygon. The CRS is also
#'     attached as a `"crs"` attribute on the returned tibble.
#' @param active_only Logical. If TRUE (default), only returns active survey blocks.
#'
#' @return Either an `sf` object or a `tbl` depending on `type`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load synoptic and HBLL survey grids as polygons (default)
#' load_survey_blocks() |>
#'   ggplot() +
#'   geom_sf(aes(fill = survey_abbrev)) +
#'   theme_minimal()
#'
#' # Load MSSM grid as centroids
#' load_survey_blocks(dataset = "mssm", type = "centroid") |>
#'   ggplot() +
#'   geom_sf(aes(colour = survey_abbrev)) +
#'   theme_minimal()
#'
#' # Load SOG grid as coordinates
#' load_survey_blocks(dataset = "syn_sog", type = "XY") |>
#'   ggplot() +
#'   geom_point(aes(x = X, y = Y)) +
#'   theme_minimal()
#' }
load_survey_blocks <- function(
  dataset = "syn_hbll",
  type = c("polygon", "centroid", "XY"),
  active_only = TRUE
) {
  type <- match.arg(tolower(type), choices = c("polygon", "centroid", "xy"))

  dataset <- match.arg(tolower(dataset), choices = c(
    "syn_hbll", "synoptic", "hbll", "hbll_ins", "hbll_out", "mssm", "syn_sog"
  ))

  # Touching the CRS forces proper initialization of these lazy-loaded sf
  # objects; without it, the *first* `[` subset on them in a session can
  # silently corrupt the geometry column (sf lazy-load/ALTREP interaction).
  invisible(sf::st_crs(gfdata::survey_blocks))
  invisible(sf::st_crs(gfdata::sog_grid))

  dat <- switch(dataset,
    "syn_hbll" = gfdata::survey_blocks,
    "synoptic" = gfdata::survey_blocks[gfdata::survey_blocks$survey_abbrev %in% c("SYN WCVI", "SYN WCHG", "SYN QCS", "SYN HS"), ],
    "hbll" = gfdata::survey_blocks[gfdata::survey_blocks$survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "HBLL OUT N", "HBLL OUT S"), ],
    "hbll_ins" = gfdata::survey_blocks[gfdata::survey_blocks$survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), ],
    "hbll_out" = gfdata::survey_blocks[gfdata::survey_blocks$survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"), ],
    "mssm" = gfdata::mssm_grid_sf |>
      dplyr::mutate(area = 9) |>
      sf::st_transform(crs = 32609) |>
      dplyr::rename(survey_abbrev = "survey"),
    "syn_sog" = gfdata::sog_grid
  )

  if (dataset != "mssm") {
    if (active_only) dat <- dat[dat$active_block, ]
  }

  if (type == "centroid") {
    return(sf::st_point_on_surface(dat)) # sf points
  }

  if (type == "xy") {
    pts <- sf::st_point_on_surface(dat)
    coords <- sf::st_coordinates(pts) / 1000  # convert metres to km
    df <- sf::st_drop_geometry(pts)
    df$X <- coords[, 1]
    df$Y <- coords[, 2]
    df <- dplyr::as_tibble(df)
    attr(df, "crs") <- sf::st_crs(pts)
    return(df)
  }

  return(dat)  # default: polygon sf
}

#' Convert SQL Geometry Data to an sf Polygon Object
#'
#' This function converts a data frame containing SQL-style geometry coordinates
#' (four corner points per polygon) into an `sf` polygon object.
#' The SQL-style geometry is returned from spatial SQL tables like SURVEY_SITE
#' and SURVEY_SITE_HISTORIC in GFBioSQL.
#'
#' @param .d A data frame with columns `pt1_lon`, `pt1_lat`, `pt2_lon`, `pt2_lat`,
#' `pt3_lon`, `pt3_lat`, `pt4_lon`, and `pt4_lat`, representing the four corners of polygons.
#'   The function expects these exact column names
#'   Additional columns will be preserved in the output.
#' @param crs An integer specifying the coordinate reference system (CRS).
#'   Defaults to `4326` (WGS 84). The function will warn if you specify WGS84 (4326)
#'   but provide coordinates that appear to be in a projected system (e.g., UTM coordinates
#'   with values outside the longitude range -180 to 180).
#'
#' @return An `sf` object with polygon geometries and original data attributes (excluding point columns).
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with data from GFBioSQL
#' .d <- gfdata::get_active_survey_blocks(active_only = TRUE)
#' sql_geom_to_sf(.d, crs = 4326)
#' }
#'
#' # Example with WGS84 coordinates (standalone, no database required)
#' wgs84_data <- data.frame(
#'   pt1_lon = -123.0, pt1_lat = 48.0,
#'   pt2_lon = -122.9, pt2_lat = 48.0,
#'   pt3_lon = -122.9, pt3_lat = 48.1,
#'   pt4_lon = -123.0, pt4_lat = 48.1,
#'   site_id = "A1"
#' )
#' sql_geom_to_sf(wgs84_data, crs = 4326)  # WGS84 coordinates
#'
sql_geom_to_sf <- function(.d, crs = 4326) {
  # Warn if coordinates seem to be in a different CRS than expected
  if (crs == 4326) {
    # Check if coordinates look like they might be in UTM or another projected system
    lon_range <- range(.d$pt1_lon, .d$pt2_lon, .d$pt3_lon, .d$pt4_lon, na.rm = TRUE)
    if (any(lon_range > 180) || any(lon_range < -180)) {
      warning("Coordinates appear to be outside WGS84 longitude range (-180 to 180). ",
              "Are you sure the input coordinates are in the specified CRS (", crs, ")?")
    }
  }

  .d$id <- seq_len(nrow(.d))
  polys <- split(.d, .d$id) |> lapply(\(x) {
    list(rbind(
      c(x$pt1_lon, x$pt1_lat),
      c(x$pt2_lon, x$pt2_lat),
      c(x$pt3_lon, x$pt3_lat),
      c(x$pt4_lon, x$pt4_lat),
      c(x$pt1_lon, x$pt1_lat)
    )) |> sf::st_polygon()
  })
  # Remove coordinate columns and temporary id column
  coord_cols <- grepl("^pt", names(.d))
  cols_to_keep <- !coord_cols & names(.d) != "id"
  out <- .d[, cols_to_keep, drop = FALSE]
  out$geometry <- sf::st_sfc(polys)
  out <- sf::st_as_sf(out)
  sf::st_crs(out) <- crs
  return(out)
}
