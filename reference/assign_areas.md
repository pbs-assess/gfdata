# Assign areas

Assign areas

## Usage

``` r
assign_areas(
  major_stat_area_description,
  area_regex = c("3[CD]+", "5[AB]+", "5[CDE]+")
)
```

## Arguments

- major_stat_area_description:

  A vector of major statistical area descriptions.

- area_regex:

  A vector of regular expressions describing the areas group.

## Examples

``` r
x <- c(
  "5D: NORTHERN HECATE STRAIT", "3C: S.W. VANCOUVER ISLAND",
  "3D: N.W. VANCOUVER ISLAND"
)
assign_areas(x)
#> [1] "5CDE" "3CD"  "3CD" 
```
