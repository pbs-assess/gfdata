# Synoptic and HBLL survey blocks

Survey blocks (grids) for DFO Pacific groundfish surveys including
Synoptic (SYN) and Hook and Line (HBLL) surveys. Obtained via
[`gfdata::get_active_survey_blocks()`](https://pbs-assess.github.io/gfdata/reference/lookup.md)
with some cleaning as documented in `data-raw/survey_blocks.R`.

## Usage

``` r
survey_blocks
```

## Format

Simple feature (`sf`) collection with 66744 features and 7 fields:

- survey_abbrev:

  Survey abbreviation. Includes:

  - `SYN QCS`: Synoptic Queen Charlotte Sound

  - `SYN HS`: Synoptic Hecate Strait

  - `SYN WCVI`: Synoptic West Coast Vancouver Island

  - `SYN WCHG`: Synoptic West Coast Haida Gwaii

  - `HBLL OUT N`: Hook and Line Outside North

  - `HBLL OUT S`: Hook and Line Outside South

  - `HBLL INS N`: Hook and Line Inside North

  - `HBLL INS S`: Hook and Line Inside South

- survey_series_id:

  Unique identifier for the survey series.

- block_id:

  Unique identifier for each grid cell (`BLOCK_DESIGNATION` in
  GFBioSQL).

- grouping_code:

  Strata grouping code used to join with strata data from the GROUPING
  table in GFBioSQL.

- depth_m:

  Depth in metres.

- active_block:

  Is block actively fished as of date downloaded: e.g.,
  `attr(gfdata::survey_blocks, "date-downloaded")`)

- geometry:

  Represents grid cell.

- area:

  Overwater area in km^2.

## See also

[`load_survey_blocks`](https://pbs-assess.github.io/gfdata/reference/load_survey_blocks.md)
for loading this dataset with additional options (polygon, centroid, or
coordinate formats).

## Examples

``` r
requireNamespace("ggplot2", quietly = TRUE)
library(sf)
library(ggplot2)
gfdata::survey_blocks |>
  dplyr::filter(active_block) |>
  ggplot(aes(colour = survey_abbrev)) +
  geom_sf() +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")

attr(gfdata::survey_blocks, "date-generated")
#> [1] "2025-07-31"
attr(gfdata::survey_blocks, "date-downloaded")
#> [1] "2025-07-30"
```
