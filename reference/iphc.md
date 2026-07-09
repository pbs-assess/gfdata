# IPHC data cleaned for use for non-halibut index standardization at PBS

These data are best used for spatiotemporal analyses. Also see the
gfiphc package which implements a design-based index standardization.
Data from 1998 onwards come from the IPHC website. Data from 1996 and
1997 come from local spreadsheets as described in gfiphc.

## Usage

``` r
iphc_sets

iphc_catch
```

## Format

Once the 2 data frames are joined, a data frame with these columns:

- year:

  Year

- station:

  Station ID

- station_key:

  A unique station key from IPHC; occasionally there are multiple sets
  at the same `station` but this column is always unique. It's also
  unique across years.

- longitude:

  Longitude (mid point)

- latitude:

  Latitude (mid point)

- species_science_name:

  Scientific name

- hooks_observed:

  Number of hooks observed for non-halibut species

- number_observed:

  Number of hooks with the species of interest

- pbs_standard_grid:

  Logical: standard grid stations through time as defined in gfiphc

- inside_wcvi:

  Logical: inside Vancouver Island waters (2018 only) vs. anywhere else;
  you may want to exclude these from spatiotemporal modelling

- sample_type:

  Sample type (first 20 hooks vs. all hooks)

- soak_time_min:

  Soak time

- temp_c:

  Temperature in degrees C

- depth_m:

  Depth in m

- species_common_name:

  Species common name

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
316956 rows and 6 columns.

## Details

One likely wants to join the data frames. E.g.

    iphc <- dplyr::inner_join(iphc_catch, iphc_sets)
