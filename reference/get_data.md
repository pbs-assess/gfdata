# Get PBS data

Automates fisheries and research survey data extraction from DFO Pacific
groundfish databases. The output datasets feed into other functions
(`tidy_`, `plot_`, or `fit_` functions) for data visualization, which
can be used as products themselves or can be fed into automated DFO
Pacific groundfish data synopsis report production.

## Usage

``` r
get_survey_sets(
  species,
  ssid = c(1, 3, 4, 16, 2, 14, 22, 36, 39, 40, 7),
  join_sample_ids = FALSE,
  verbose = FALSE,
  sleep = 0.05
)

get_ll_hook_data(species = NULL, ssid = NULL)

get_survey_samples(
  species,
  ssid = NULL,
  remove_bad_data = TRUE,
  unsorted_only = TRUE,
  usability = NULL,
  major = NULL
)

get_commercial_samples(
  species,
  unsorted_only = TRUE,
  return_all_lengths = FALSE,
  major = NULL,
  usability = NULL
)

get_catch(species, major = NULL)

get_cpue_historical(
  species = NULL,
  major = NULL,
  alt_year_start_date = "04-01",
  areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
  end_year = NULL
)

get_cpue_historical_hake(end_year = NULL)

get_cpue_historical_hl(
  species = NULL,
  major = NULL,
  alt_year_start_date = "04-01",
  areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
  end_year = NULL
)

get_cpue_spatial(species, major = NULL)

get_catch_spatial(species, major = NULL)

get_cpue_spatial_ll(species, major = NULL)

get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996, major = NULL)

get_cpue_index_hl(min_cpue_year = 1980, major = NULL)

get_age_precision(species, major = NULL)

get_survey_index(species, ssid = NULL)

get_sable_landings(species, ssid = NULL)

get_survey_blocks(ssid = NULL)

get_eulachon_specimens()

get_gear_types()

get_management(
  species = NULL,
  species_group = NULL,
  fishery = NULL,
  area = NULL,
  start_year = NULL
)

cache_pbs_data(
  species,
  major = NULL,
  file_name = NULL,
  path = ".",
  compress = FALSE,
  unsorted_only = TRUE,
  historical_cpue = FALSE,
  survey_sets = FALSE,
  verbose = TRUE,
  return_all_lengths = FALSE,
  ssid = NULL
)

get_hake_survey_samples()

get_hake_catch(end_date = format(Sys.Date(), "%d/%m/%Y"))

get_sablefish_surveys()
```

## Arguments

- species:

  One or more species common names (e.g. `"pacific ocean perch"`) or one
  or more species codes (e.g. `396`). Species codes can be specified as
  numeric vectors `c(396, 442`) or characters `c("396", "442")`. Numeric
  values shorter than 3 digits will be expanded to 3 digits and
  converted to character objects (`1` turns into `"001"`). Species
  common names and species codes should not be mixed. If any element is
  missing a species code, then all elements will be assumed to be
  species common names.

- ssid:

  A numeric vector of survey series IDs. Run
  [`get_ssids()`](https://pbs-assess.github.io/gfdata/reference/lookup.md)
  for a look-up table of available survey series IDs with surveys series
  descriptions.

- join_sample_ids:

  If `TRUE` then the sample IDs will be joined in. This may result in
  repeated rows of data if the same sample ID is part of different
  survey stratifications.

- verbose:

  If `TRUE` then extra messages were reprinted during data extraction.
  Useful to monitor progress.

- sleep:

  System sleep in seconds between each survey-year to be kind to the
  server.

- remove_bad_data:

  Remove known bad data, such as unrealistic length or weight values.

- unsorted_only:

  Remove sorted biological data ('keepers' and 'discards' and unknown).
  Default = TRUE.

- usability:

  A vector of usability codes to include. Defaults to all. IPHC codes
  may be different to other surveys.

- major:

  To select only the inside population (Strait of Georgia, area 4B
  only), set inside = 1. To select only the outside population, set
  inside = 0.

- return_all_lengths:

  Include all length types, rather than just with most common
  measurement. Default = FALSE.

- alt_year_start_date:

  Alternative year starting date specified as a month-day combination.
  E.g. "03-01" for March 1st. Can be used to create 'fishing years'.

- areas:

  Area groupings as a vector of regular expressions. See
  [`base::regex()`](https://rdrr.io/r/base/regex.html).

- end_year:

  Specify the last calendar year to be extracted.

- gear:

  The gear type(s) to include for CPUE. Will be converted to uppercase.
  Run
  [`get_comm_gear_types()`](https://pbs-assess.github.io/gfdata/reference/lookup.md)
  for a look-up table of available gear types to select from.

- min_cpue_year:

  Minimum year for the CPUE data.

- species_group:

  Species group code(s) to include (see lookup table
  [`get_species_groups()`](https://pbs-assess.github.io/gfdata/reference/lookup.md)).
  Defaults to all.

- fishery:

  The fishery_id code(s) (see lookup table
  [`get_fishery_ids()`](https://pbs-assess.github.io/gfdata/reference/lookup.md))
  for fisheries to include in data extraction. Defaults to all.

- area:

  The fishery area(s) (see lookup table
  [`get_management_areas()`](https://pbs-assess.github.io/gfdata/reference/lookup.md))
  to include in data extraction (eg. '5A'; c('3C', '3D', '5A', '5B')).

- start_year:

  The minimum year to include management actions. Defaults to all.

- file_name:

  Optional filename(s) for the cached file. Defaults to the same as the
  `species` argument.

- path:

  The folder where the cached data will be saved.

- compress:

  Compress the `.rds` file? Defaults to `FALSE` for faster reading and
  writing at the expense of disk space.

- historical_cpue:

  Logical for whether historical CPUE should be included.

- survey_sets:

  Logical for whether the survey set data should be extracted. You might
  set this to `FALSE` if you don't need these data and you want to
  substantially speed up data extraction.

- end_date:

  A string representing the date. Must be of the format dd/mm/yyyy

## Value

The `get_*` functions return a data frame. The `cache_pbs_data()`
function writes an `.rds` file to `path` for each specified species. A
data object for a single species is a named list object with each
element containing a data frame from a `get_*` function. The element
name of the list reflects the function name with the `get_` part
removed. For example, the output from `get_survey_samples()` is in a
list element named `survey_samples()`.

## Details

- `get_survey_sets()` extracts survey catch data and spatial data for
  plotting survey catchs on a map of British Columbia

- `get_survey_samples()` extracts all biological sample specimen records
  from research surveys for given species and survey series IDs from
  GFBio

- `get_hake_survey_samples()` extracts all biological sample specimen
  records from hake joint acoustic surveys from GFBio

- `get_commercial_samples()` extracts all biological sample specimen
  records from commercial data for given species from GFBio

- `get_catch()` extracts all landing and discard records for a given
  species from GFFOS.GF_MERGED_CATCH

- `get_hake_catch()` extracts all landing and discard records for
  Pacific Hake with some extra data used in the Hake assessment

- `get_cpue_spatial()` extracts catch, effort and spatial data from
  GFFOS.GF_D_OFFICIAL_CATCH for the groundfish trawl fishery

- `get_cpue_spatial_ll()` extracts catch, effort and spatial data from
  GFFOS.GF_D_OFFICIAL_CATCH for the longline fishery

- `get_cpue_index()` extracts catch and effort data from
  GFFOS.GF_MERGED_CATCH for the groundfish trawl fishery since 1996

- `get_cpue_historical()` extracts historical catch and effort data back
  into the 1950s. It's help file is on a separate page; see the link

- `get_age_precision()` extracts age readings from biological samples
  for a given species where there is a second ('precision') age reading

- `get_sara_dat()` scrubs Species At Risk website for up-to-date species
  status and listings

- `get_survey_index()` extracts survey catch data for given species and
  survey series IDs

- `get_management()` extracts management actions

- `cache_pbs_data()` runs all 'get' functions in the gfdata package

  (except those specific to IPHC data) and caches extracted data to a
  given folder

This `cache_pbs_data()` function caches data from

- `get_survey_samples()`

- `get_commercial_samples()`

- `get_catch()`

- `get_cpue_spatial()`

- `get_cpue_spatial_ll()`

- `get_catch_spatial()`

- `get_survey_index()`

- `get_age_precision()`

- and optionally from `get_survey_sets()` and `get_cpue_historical()`

## Authentication

`get_*` functions only extract data when performed on a computer
connected to the Pacific Biological Station DFO network. By default, the
functions assume that you are on an authorized DFO Windows computer
where authentication with the databases happens automatically. If
instead, you wish to connect by username and password, see the details
section in
[`run_sql()`](https://pbs-assess.github.io/gfdata/reference/run_sql.md).

## Examples

``` r
if (FALSE) { # \dontrun{
## Import survey catch density and location data by tow or set for plotting
## Specify single or multiple species by common name or species code and
## single or multiple survey series id(s).
get_survey_sets(species = "lingcod", ssid = 1)

## Import survey or commercial biological data for various plots
## (e.g. length frequency, growth, age frequency, maturity, etc.)
get_survey_samples(species = 442, ssid = c(1, 3, 4, 16))

get_commercial_samples(c(442, 397))

## Import catch data by species for barcharts of landings by fishing area,
## geartype, and year.
get_catch("lingcod")

## Import spatial commercial catch per unit effort data for trawl or longline
## data by species for plotting along BC coast.
get_cpue_spatial("lingcod")
get_cpue_spatial_ll("yelloweye rockfish")

## Import catch and effort data by gear type for modelling commercial trawl
## cpue index.
get_cpue_index(gear = "bottom trawl", min_cpue_year = 2012)

## Import survey bootstrapped biomass estimates for plotting relative biomass
## indices by specified survey series.
get_survey_index("pacific cod", ssid = c(1, 3, 4, 16))
} # }
```
