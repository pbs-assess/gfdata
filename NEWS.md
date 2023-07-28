# Updates to gfdata

## 2023-07-13
- HBLL INS (ssid: 39, 40) are now included in the default call for `get_survey_sets()`

## 2019-05-2019
- Year, month and catch calculation moved to sql code instead of tidy funcion 
to reduce run time on already extracted data.
- Beginning to adapt tidy_cpue_index to work for hook and line data in additon
to trawl data.
