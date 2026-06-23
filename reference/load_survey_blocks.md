# Load survey block data (polygon, point, or coordinate table)

Returns built-in grid datasets as either polygons, centroids, or
coordinates. Available datasets include the Synoptic and HBLL survey
grids (2x2 km square grids), MSSM grid, and SYN SOG grid. Note that
centroid and coordinate outputs may fall on land rather than in the
ocean. While suitable for visualization and basic modeling, these points
should not be used directly for extracting oceanographic covariates -
instead use `polygon` and extract as appropriate.

## Usage

``` r
load_survey_blocks(
  dataset = "syn_hbll",
  type = c("polygon", "centroid", "XY"),
  active_only = TRUE
)
```

## Arguments

- dataset:

  Character string specifying the dataset to load. One of:

  - `"syn_hbll"` (default): Synoptic and HBLL survey grids (2x2 km
    square grids that may overlap with land).

  - `"mssm"`: MSSM survey grid data, see
    [`gfdata::mssm_grid`](https://pbs-assess.github.io/gfdata/reference/mssm_grid.md).

  - `"syn_sog"`: Strait of Georgia Synoptic Bottom Trawl grid (only
    active blocks available).

- type:

  Character string specifying the output format. One of:

  - `"polygon"` (default): returns an `sf` object with polygon
    geometries.

  - `"centroid"`: returns an `sf` object with the centroid point for
    each block.

  - `"XY"`: returns a `tibble` with columns `X` and `Y` (in kilometres),
    representing point-on-surface coordinates extracted from each
    polygon.

- active_only:

  Logical. If TRUE (default), only returns active survey blocks.

## Value

Either an `sf` object or a `tbl` depending on `type`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load synoptic and HBLL survey grids as polygons (default)
load_survey_blocks() |>
  ggplot() +
  geom_sf(aes(fill = survey_abbrev)) +
  theme_minimal()

# Load MSSM grid as centroids
load_survey_blocks(dataset = "mssm", type = "centroid") |>
  ggplot() +
  geom_sf(aes(colour = survey_abbrev)) +
  theme_minimal()

# Load SOG grid as coordinates
load_survey_blocks(dataset = "syn_sog", type = "XY") |>
  ggplot() +
  geom_point(aes(x = X, y = Y)) +
  theme_minimal()
} # }
```
