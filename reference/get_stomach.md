# Get stomach contents

Get stomach contents

## Usage

``` r
get_survey_stomachs(
  ssid = NULL,
  unsorted_only = FALSE,
  usability = NULL,
  major = NULL
)

get_all_stomachs(unsorted_only = FALSE, major = NULL, usability = NULL)
```

## Arguments

- ssid:

  A numeric vector of survey series IDs. Run
  [`get_ssids()`](https://pbs-assess.github.io/gfdata/reference/lookup.md)
  for a look-up table of available survey series IDs with surveys series
  descriptions.

- unsorted_only:

  Remove sorted biological data ('keepers' and 'discards' and unknown).
  Default = FALSE. IPHC codes may be different to other surveys.

- usability:

  A vector of usability codes to include. Defaults to all.

- major:

  Character string (or vector, though doesn't work yet with
  `cache_pbs_data`) of major stat area code to include (characters). Use
  get_major_areas() to lookup area codes with descriptions.
