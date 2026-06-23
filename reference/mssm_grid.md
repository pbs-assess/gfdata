# Multispecies Small-Mesh Bottom Trawl (MSSM) Survey Grid

A 3x3 km grid for the Multispecies Small-Mesh Bottom Trawl Survey (MSSM;
formerly known as 'shrimp survey'). This grid covers WCVI Shrimp Survey
Areas 124 and 125. The `year` is the last year a grid cell was sampled
as of 2023, with the most consistent resampling (spatially) occurring in
grid cells last sampled between 2009 and 2021.

## Usage

``` r
mssm_grid

mssm_grid_sf
```

## Format

### `mssm_grid`

A data frame with 3,735 rows and 5 columns:

- survey_abbrev:

  Survey abbreviation

- longitude, latitude:

  Longitude and latitude of the centroid of 3x3 km grid cells

- area:

  Area of the grid cells, in km^2

- year:

  The year a grid cell was sampled. A grid cell can have multiple year
  values

### `mssm_grid_sf`

A simple features (`sf` object) version of `mssm_grid`

An object of class `sf` (inherits from `tbl_df`, `tbl`, `data.frame`)
with 3735 rows and 3 columns.

## See also

[`load_survey_blocks`](https://pbs-assess.github.io/gfdata/reference/load_survey_blocks.md)
for loading this dataset with additional options (polygon, centroid, or
coordinate formats).
