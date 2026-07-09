# Calculate a design index

Calculate a design-based index with bootstrapped standard errors

## Usage

``` r
get_design_index(species, ssid = NULL, reps = 1000, data = NULL)
```

## Arguments

- species:

  Species common name or number

- ssid:

  Survey series ID

- reps:

  Number of bootstrap samples. Set to `0` to skip the bootstrapping and
  only report design-based variance estimates.

- data:

  Optional output from a call to
  [`get_all_survey_sets()`](https://pbs-assess.github.io/gfdata/reference/get_all.md).
  By default, this function will call
  [`get_all_survey_sets()`](https://pbs-assess.github.io/gfdata/reference/get_all.md)
  for the given species, but this requires access to the GFBio database,
  and will also be slower if you've already cached the data.
