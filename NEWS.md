# Updates to gfdata

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
