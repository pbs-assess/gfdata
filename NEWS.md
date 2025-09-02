# Updates to gfdata

## 0.1.6 2025-09-02

- Added `sql_geom_to_sf()` utility function for converting SQL geometry data to sf objects (e.g., results from `get_active_survey_blocks()`)
- Fixed survey block lookup query to get GFBioSQL SURVEY_SITE table results for a given ssid

## 0.1.5 2025-07-09

- Added `load_survey_blocks()` function to return survey block data in the following formats:
 - `"polygon"` (default): `sf` object with 2x2 km grid polygon geometries
 - `"centroid"`: `sf` object with centroid points for each block
 - `"XY"`: tibble with X/Y coordinates in kilometres
- Added `active_only` parameter (default: `TRUE`) to optionally filter for active survey blocks
- Note: centroid and XY coordinate outputs may fall on land (specifically for the HBLL surveys); use polygon format for oceanographic data extraction
- Updated `survey_blocks` documentation to correctly reflect the actual data structure

## 0.1.4 2025-01-22

2024 IPHC data have been added. Since 2024-05 the following changes have occurred

- `Bait` column added; but this does not resolve the baits used in the 2012 
  bait experiment, these 2012 values are NA

- Some missing values in environmental data columns are now included

- Additional non halibut species were added, this seems to be the inclusion of
  extra stations, mostly in 2018 and 2020 that did not have data before

- Widow rockfish are now in the time series because one was caught in 2018 in a
  set that has been recently added

- The number of halibut observed station_key == 20220349, was decreased by one
  (56 -> 55)



## 0.1.4 2024-12-20

- Comment out SQL that was dropping midwater trawl discard weights before 2006.
  See line in `inst/sql/get-catch.sql` about trip category `OPT B`.
  Currently line 22. We see no reason to remove these.


## 0.1.3 2024-07-17

- For commercial biological samples: discards with a null sample source code are
  now coded as discards instead of unknown.

## 2023-07-13

- HBLL INS (ssid: 39, 40) are now included in the default call for `get_survey_sets()`

## 2019-05-2019

- Year, month and catch calculation moved to sql code instead of tidy funcion 
to reduce run time on already extracted data.
- Beginning to adapt tidy_cpue_index to work for hook and line data in additon
to trawl data.
