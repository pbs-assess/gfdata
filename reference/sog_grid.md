# Strait of Georgia Synoptic Bottom Trawl Survey Grid

Survey blocks for the Strait of Georgia (SOG) Synoptic Bottom Trawl
Survey. This dataset contains only active survey blocks and represents
the grid used for the Strait of Georgia synoptic survey (survey series
ID 45).

## Usage

``` r
sog_grid
```

## Format

Simple feature (`sf`) collection with 7 fields:

- survey_series_id:

  Unique identifier for the survey series (45 for SOG).

- survey_abbrev:

  Survey abbreviation: "SYN SOG".

- block_id:

  Unique identifier for each grid cell (`BLOCK_DESIGNATION` in
  GFBioSQL).

- grouping_code:

  Strata grouping code used to join with strata data from the GROUPING
  table in GFBioSQL.

- depth_m:

  Depth in metres.

- active_block:

  Is block actively fished (all blocks in this dataset are active).

- area:

  Overwater area in km^2.

- geometry:

  Represents grid cell.

## See also

[`load_survey_blocks`](https://pbs-assess.github.io/gfdata/reference/load_survey_blocks.md)
for loading this dataset with additional options (polygon, centroid, or
coordinate formats).

## Examples

``` r
requireNamespace("ggplot2", quietly = TRUE)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(ggplot2)
gfdata::sog_grid |>
  ggplot() +
  geom_sf(aes(fill = depth_m)) +
  theme_minimal() +
  scale_fill_viridis_c(name = "Depth (m)")

attr(gfdata::sog_grid, "date-generated")
#> [1] "2025-09-04"
attr(gfdata::sog_grid, "date-downloaded")
#> [1] "2025-09-04"
```
