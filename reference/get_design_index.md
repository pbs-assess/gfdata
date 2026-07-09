# Calculate a design index

Calculate a design-based index with bootstrapped standard errors

## Usage

``` r
get_design_index(species, ssid = NULL, reps = 1000)
```

## Arguments

- species:

  Species common name or number

- ssid:

  Survey series ID

- reps:

  Number of bootstrap samples. Set to `0` to skip the bootstrapping and
  only report design-based variance estimates.
