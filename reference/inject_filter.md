# gfdata utilities

gfdata utilities

## Usage

``` r
inject_filter(
  sql_precode,
  species,
  sql_code,
  search_flag = "-- insert species here",
  conversion_func = common2codes
)
```

## Arguments

- sql_precode:

  SQL go to inject before a species list etc.

- species:

  A vector of species or similar objects.

- sql_code:

  The SQL go to operate on.

- search_flag:

  What to search for in terms of where to inject the code.

- conversion_func:

  A conversion function to apply to the species or related vector.
