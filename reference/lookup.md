# Lookup PBS metadata and descriptive data filters

Extracts metadata and descriptive details useful in filtering data (i.e.
for extracting data only from a specific survey). Codes are often used
in arguments or for filtering and can be looked up with these functions.

## Usage

``` r
get_ssids()

get_active_survey_blocks(ssid = NULL, active_only = TRUE)

get_major_areas()

get_management_areas()

get_fishery_ids()

get_species_groups()

get_comm_gear_types()

get_survey_gear_types()

get_age_methods()

get_species()

get_strata_areas()

get_survey_ids(ssid)

get_sensor_attributes()

get_fishery_sectors()

get_other_surveys()

get_table(name)
```

## Arguments

- ssid:

  A numeric vector of survey series IDs. If NULL (default), returns
  grids for Synoptic trawl surveys (1, 3, 4, 16), HBLL Outside surveys
  (22, 36), and HBLL Inside surveys (39, 40). For all available survey
  series, first run `get_ssids()` to see options, then pass the desired
  IDs to this function.

- active_only:

  Logical: return only active blocks? (default: TRUE)

- name:

  The name of the table to get all records from. For code table use
  variable name without the'\_code' suffix.

## Value

A data frame of survey blocks with columns for survey series ID, name,
block designation, coordinates, and selection indicators.

## Details

- `get_ssids()` produces a lookup table for survey series IDs and
  descriptions

- `get_age_methods()` produces a lookup table for ageing method codes
  and descriptions

- `get_sample_trips()`produces a lookup table for sample ID and fishing
  event ID

- `get_strata_areas()` produces a lookup table for surveyed area for
  each stratum within surveys

- `get_survey_ids()` produces lookup table for survey IDs for a given
  survey series ID

- `get_major_areas()` produces lookup table for major area descriptions
  for a given major area code

## Note

- ssid 2 (Hecate Strait Multispecies Assemblage Bottom Trawl) is not
  available in SURVEY_SITES

- ssids 6 and 7 (Small mesh Multispecies bottom trawl for QCS and WCVI)
  are stored as SURVEY_SERIES_ID = 67, but this grid is best generated
  from sampling points if used for modelling

- ssid 14 (IPHC) contains only 4 large polygons spanning the IPHC survey
  area and is also best generated from sampling points for modelling
