# Design-based survey indexes

Design-based indexes as calculated from survey set data. A bootstrap
with 20,000 replicates is used for calculation of standard errors and
confidence intervals. See file `data-raw/design-indexes.R` for
calculation. This is the output from running
[`get_design_index()`](https://pbs-assess.github.io/gfdata/reference/get_design_index.md)
on all the species in the groundfish data synopsis report.

## Usage

``` r
design_indexes
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
16675 rows and 8 columns.

## See also

[`get_design_index()`](https://pbs-assess.github.io/gfdata/reference/get_design_index.md)
