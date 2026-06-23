# Load and join IPHC FISS set and catch data

These data start with year 1996.

## Usage

``` r
load_iphc_dat(species = NULL)
```

## Arguments

- species:

  Optional vector specifying the `species_common_name` to subset the
  IPHC data.

## Value

A tibble with 273,714 rows and 22 columns, containing the processed IPHC
catch and set data. The main columns include:

- year. Year the set was hauled.

- station. Permanent station ID named by IPHC.

- station_key. Unique key for the set; can be used to join set table to
  non-Pacific halibut catch data from FISS Survey download page.
  Occasionally there are multiple sets at the same `station` but this
  column is always unique. It's also unique across years.

- species_common_name. Species name used in GFBio.

- species_science_name. Scientific name used in GFBio.

- number_observed. Number of hooks with the species of interest. Some
  species such as skates and shortspine thornyheads were not enumerated
  to the species level in early years and so these values are `NA`.

- longitude. Longitude in decimal degrees of the midpoint of the set.

- latitude. Latitude in decimal degrees of the midpoint of the set.

- usable. IPHC indication of whether station was deemed effective ("Y")
  or ineffective ("N") for assessment.

- hooks_retrieved. Number of hooks retrieved.

- hooks_observed. Number of hooks observed for non-Pacific halibut
  species catch. For Pacific halibut this is the value of
  `hooks_retrieved`.

- pbs_standard_grid. Stations defined as 'standard' in `gfiphc`.

- inside_wcvi. Logical: inside Vancouver Island waters (2018 only) vs.
  anywhere else; you may want to exclude these from spatiotemporal
  modelling.

- sample_type. Type of observations.

  - "20 hooks" - Observation of the first 20 (non-halibut) hooks of each
    skate.

  - "all hooks" - All hooks observed.

- depth_m. Average of beginning and end depth of set in metres.

- temp_c. Temperature at profiler max pressure (degrees Celsius).

- soak_time_min. Time interval gear was in water (minutes).

- avg_no_hook_per_skate. Average number of hooks per skate at setting.

- no_skates_hauled. Number of skates hauled (no metadata on IPHC
  website).

- no_skates_set. Number of skates set, not adjusted for baits or average
  number of hooks per skate.

- effective_skates. When all hooks are observed, this is the effective
  skate number found in the raw IPHC FISS set data. For 1995-1997 this
  value comes from `gfiphc`. For cases where only a subset of hooks are
  observed, the effective skate based on all observed hooks is scaled
  as: `effective_skates * (hooks_observed / hooks_retrieved)`. See eqn
  G.4 in Anderson et al. (2019).

- baits_returned. The number of baited hooks remaining. These are
  unavailable (NA) for some Pacific halibut records where the
  sample_type = '20 hooks'.

The tibble also contains the following attributes:

- iphc_download_date. Date when the data was downloaded, e.g.,
  "2024-05-23".

- data_preparation_date. Date the data was prepared, e.g., "2024-06-11".

## Details

This function joins the IPHC catch data with the set metadata. This is
needed because Pacific halibut are enumerated for all hooks_retrieved in
a year but non-halibut species are only enumerated for hooks_observed in
a year. The catch and set dataframes are saved separately for space
efficiency. Note that in 2012 a bait experiment was run where the
typically used chum bait was only used on only 4 skates (see Appendix
G.3 Anderson et al. 2019 and Henry et al. 2013). Therefore we have
estimated the hooks_observed for Pacific halibut in 2012 as
`avg_no_hook_per_skate * .data$no_skates_hauled` and because the IPHC
has `effective_skates` = 0, this is returned here as `NA`. This total
estimate of hooks observed for Pacific halibut is therefore for all
baits, not chum-only.

## References

Anderson, S.C., E.A. Keppel, A.M. Edwards. 2019. A reproducible data
synopsis for over 100 species of British Columbia groundfish. DFO Can.
Sci. Advis. Sec. Res. Doc. 2019/041. vii + 321 p.

Henry, E., Soderlund, E., Dykstra, C.L., Geernaert, T.O., and Ranta,
A.M. 2013. 2012 standardized stock assessment survey. In Int. Pac.
Halibut Comm. Report of Assessment and Research Activities 2012. pp.
503–538.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve and process the IPHC data
iphc_data <- load_iphc_dat()
} # }
```
