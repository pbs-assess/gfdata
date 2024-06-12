# Goal:
# Create a code pipeline from IPHC website .xlsx file downloads for
# all years combined into a dataset for non-halibut species that can be used
# for spatiotemporal index standardization and matches the PBS database
# species names.

# Instructions for updating in future years:
# - head to <https://www.iphc.int/data/fiss-survey-raw-survey-data/>
# - expand the year range from 1998 to current year
# - from the dropdown, select '2B' from 'IPHC Regulatory Areas'
# - leave everything else at defaults
# - click the little square with a down arrow towards the bottom
# - select 'Crosstab'
# - leave the default Excel format
# - click on 'Non-Pacific halibut data' and wait for the download
# - repeat for 'Set and Pacific halibut data'
# - move these 2 .xlsx files into this folder
# - Set the date you did this download here:
DOWNLOAD_DATE <- "2024-05-23"
# - run the following code

# There shouldn't be any tweaks needed unless the IPHC makes changes to their
# survey design (which I'm sure they will!).
# Inspect the plots at the end and make sure you're happy with the data.

library(dplyr)
library(ggplot2)
devtools::load_all("../gfsynopsis")
theme_set(theme_light())

# read in and clean up IPHC .xlsx downloads ---------------------------------
# load station meta-data and halibut data
# all stations; 2B
if (file.exists("data-raw/Set and Pacific halibut data.xlsx")) {
  set <- readxl::read_xlsx("data-raw/Set and Pacific halibut data.xlsx")
  saveRDS(set, file = "data-raw/Set and Pacific halibut data.rds")
} else {
  set <- readRDS("data-raw/Set and Pacific halibut data.rds")
}

names(set) <- tolower(names(set))
names(set) <- gsub(" ", "_", names(set))
names(set) <- gsub("\\(", "", names(set))
names(set) <- gsub("\\)", "", names(set))
names(set) <- gsub("\\-", "_", names(set))
names(set) <- gsub("\\/", "_per_", names(set))
names(set) <- gsub("\\.", "", names(set))
names(set) <- gsub("___", "_", names(set))
set$date <- lubridate::dmy(set$date)

# download 2B:
if (file.exists("data-raw/Non-Pacific halibut data.xlsx")) {
  d <- readxl::read_xlsx("data-raw/Non-Pacific halibut data.xlsx")
  saveRDS(d, file = "data-raw/Non-Pacific halibut data.rds")
} else {
  d <- readRDS("data-raw/Non-Pacific halibut data.rds")
}
names(d) <- tolower(names(d))
names(d) <- gsub(" ", "_", names(d))
d$scientific_name <- tolower(d$scientific_name)
d$species_name <- tolower(d$species_name)
# Out of date scientific names in raw IPHC FISS data
d$scientific_name[d$scientific_name == "raja binoculata"] <- "beringraja binoculata"
d$scientific_name[d$scientific_name == "bathyraja kincaida"] <- "bathyraja interrupta"
d$scientific_name[d$scientific_name == "galeorhinus zyopterus"] <- "galeorhinus galeus" # tope shark
d$scientific_name[d$scientific_name == "delolepis gigantia"] <- "cryptacanthodes giganteus" # giant wrymouth
d$scientific_name[d$scientific_name == "eopsetta exilis"] <- "lyopsetta exilis" # slender sole

# baited hook count is used for Watson et al. 2023 censoring approach
baits_returned <- set |>
  distinct(year, stlkey, station) |>
  left_join(
    d |>
      filter(species_name == "hook with bait") |>
      select(stlkey, number_observed)
   ) |>
   mutate(baits_returned = if_else(is.na(number_observed), 0, number_observed)) |>
   left_join(distinct(d, year, sampletype)) |>
   rename(baited_hook_sampletype = "sampletype") |> # need this info to make decisions for halibut catch
   select(-number_observed)

# get halibut data and match format of `d`
hal_count_dat <- set |>
  mutate(scientific_name = "hippoglossus stenolepis",
         species_name = "pacific halibut",
         number_observed = o32_pacific_halibut_count + u32_pacific_halibut_count) |>
  select(any_of(c(names(d), "no_skates_set", "no_skates_hauled", "avg_no_hook_per_skate", "effective_skates_hauled"))) |>
  select(-row_number) |>
  left_join(x = _,
            y = distinct(d, stlkey, setno, sampletype, hooksfished, hooksretrieved, hooksobserved)) |> # need hook counts from non-halibut data
  filter(!is.na(hooksfished)) # omt stations where no other species were seemingly sampled
  # mutate(hooksfished = avg_no_hook_per_skate * no_skates_hauled) # except for sets where no non-halibut were captured...

d <- bind_rows(d, hal_count_dat) |>
  left_join(baits_returned)

# Additional species clean up --------------------------------------------------
# --- Rougheye/blackspotted complex ---
# d |>
#   filter(stringr::str_detect(species_name, "rougheye|blackspotted|shortraker")) |>
#   janitor::tabyl(species_name)
# The blackspotted rockfish first appears in 2020 see: https://github.com/pbs-assess/gfiphc/issues/17
# Before that gfiphc and GFBio had rougheye rockfish listed as rougheye/blackspotted
# so for now we will continue to do this and omit the rougheye/shortraker occurences
# which have only occured 5 times so far, with only 9 total observed to date (2023)
# filter(d, species_name == "rougheye/shortraker") |> glimpse()
#d$scientific_name[d$scientific_name %in% c("sebastes aleutianus", "sebastes melanostictus")] <- "sebastes aleutianus/melanostictus complex"
re_bs <- filter(d, scientific_name %in% c("sebastes aleutianus", "sebastes melanostictus")) |>
  mutate(scientific_name = "sebastes aleutianus/melanostictus complex") |>
  group_by(year, stlkey, station, setno, scientific_name,
    sampletype, hooksfished, hooksretrieved, hooksobserved,
    no_skates_set, no_skates_hauled, avg_no_hook_per_skate,
    effective_skates_hauled, baits_returned, baited_hook_sampletype) |>
  summarise(number_observed = sum(number_observed)
  ) |> ungroup()
# Replace rougheye/blackspotted rows with the summed counts of rougheye and blackspotted
d <- d |>
  filter(!(scientific_name %in% c("sebastes aleutianus", "sebastes melanostictus"))) |>
  bind_rows(re_bs)

# --- Pacific Sand Dab ---
# See also: https://github.com/pbs-assess/gfiphc/blob/master/inst/extdata/iphc-spp-names.csv
# "GFbio also has speckled, Maria says IPHC ones are likely Pacific sand dab"
d$scientific_name[d$scientific_name == "citharichthys spp"] <- "citharichthys sordidus"

# --- Southern Rock Sole ---
# See also: https://github.com/pbs-assess/gfiphc/blob/master/inst/extdata/iphc-spp-names.csv
# in GFBio and gfiphc, rock sole has been considered to be southern rock sole
d$scientific_name[d$scientific_name == "lepidopsetta sp."] <- "lepidopsetta bilineata"

# --- Pacific Grenadier ---
# In the past, GFBio and gfiphc appear to have scientific_name == "macrouridae",
# classified as pacific grenadier (coryphaenoides acrolepis). But it is unclear
# if this is what we should continue doing
#d$scientific_name[d$scientific_name == "macrouridae"] <- "coryphaenoides acrolepis"

spp <- gfsynopsis::get_spp_names()
missing <- filter(d, !scientific_name %in% spp$species_science_name)
sort(unique(missing$species_name))
# - blackspotted rockfish [ignore]
# - rougheye rockfish [ignore]

count_dat <- filter(d, scientific_name %in% spp$species_science_name)
sort(unique(count_dat$species_name))

if (FALSE) {
  # a helper to find scientific names to fix above
  TEST <- "sandpaper skate"
  filter(d, species_name == TEST) |>
    select(scientific_name) |>
    unique()
  filter(spp, species_common_name == TEST) |> select(species_science_name)
}

count_dat <- transmute(count_dat,
  year = as.integer(year),
  species_common_name = species_name,
  species_science_name = scientific_name,
  hooks_fished = as.integer(hooksfished), # excluding for now
  hooks_retrieved = as.integer(hooksretrieved), # excluding for now
  hooks_observed = as.integer(hooksobserved),
  number_observed = as.integer(number_observed),
  sample_type = if_else(sampletype == "20Hook", "20 hooks", "all hooks"),
  # set_number = as.integer(setno), # excluding for now
  station = as.integer(station),
  station_key = as.integer(stlkey),
  baits_returned = as.integer(baits_returned)
)

# need to collapse bering skate and sandpaper skate into one species:
# bathyraja interrupta
count_dat <- count_dat |>
  group_by(year, species_science_name, sample_type, station, station_key) |>
  summarise(
    hooks_fished = sum(hooks_fished),
    hooks_retrieved = as.integer(hooks_retrieved),
    hooks_observed = sum(hooks_observed),
    number_observed = sum(number_observed),
    baits_returned = sum(baits_returned),
    species_common_name = species_common_name[1], # pick one for now; the DFO ones get joined anyways
    .groups = "drop"
  )

set |>
  ggplot(aes(midlon_fished, midlat_fished, colour = purpose_code)) +
  geom_point() +
  facet_wrap(~year)

table(set$purpose_code)
# set <- filter(set, purpose_code == "Standard Grid")
set_dat <- transmute(set,
  year = as.integer(year),
  station_key = as.integer(stlkey),
  station = as.integer(station),
  longitude = midlon_fished,
  latitude = midlat_fished,
  depth_m = 1.8288 * avgdepth_fm,
  usable = eff,
  soak_time_min,
  effective_skates = effective_skates_hauled,
  avg_no_hook_per_skate = avg_no_hook_per_skate,
  no_skates_set = no_skates_set,
  no_skates_hauled = no_skates_hauled,
  temp_c
)

set_dat |>
  ggplot(aes(longitude, latitude)) +
  geom_point() +
  facet_wrap(~year)

# think about skates and hooks observed:
dat_test <- inner_join(count_dat, set_dat, by = join_by(year, station, station_key))
ggplot(dat_test, aes(avg_no_hook_per_skate * no_skates_hauled, hooks_observed, colour = sample_type)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0.2) + geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~year)

# what's up with 2012!?
filter(dat_test, year == 2012) |> select(avg_no_hook_per_skate, no_skates_hauled, hooks_observed, effective_skates) |>
  distinct()
# stated to be 'all' hooks observed,
# *but* it actually looks like half of hooks were observed
# and "effective skates" is all 0!?
# gfiphc (GFBio) actually has effective skates for these
# but they look to be based on the 'half' measurement (around 4 vs. 8 skates hauled)
# so, let's leave them as is in the data according to hooks observed...

# need this to be a right join because very occasionally there are no non-halibut catch!
dat <- right_join(count_dat, set_dat, by = join_by(year, station, station_key)) |>
# Let's add the effective skates in because they are still used for the offset calculation
# For these missing 2012 years we'll use the approximation: hooks_observed / avg_no_hook_per_skate
  mutate(effective_skates = ifelse((year == 2012 & effective_skates == 0), hooks_observed / avg_no_hook_per_skate, effective_skates))
#dat <- left_join(count_dat, set_dat, by = join_by(year, station, station_key))

# need to fill in the zeros -------------------------------------------------

# # now pull out any rows that didn't have any non-halibut species;
# # we need to fill those in with zeros for all non-halibut species
# # and they're currently missing sample_type and hooks_observed!
# never_caught_non_halibut <- filter(dat, is.na(hooks_observed))
# # need to fill in those NAs for hook counts; do it from avg. hooks and no. skates hauled
# # first need to fill in the sample types, which will also be NAs here
# # so bring them back over:
# never_caught_non_halibut$sample_type <- NULL
# never_caught_non_halibut <- left_join(never_caught_non_halibut,
#   filter(dat, !is.na(sample_type)) |> select(year, sample_type) |> distinct(), by = join_by(year))
# never_caught_non_halibut <- never_caught_non_halibut |>
#   mutate(hooks_observed =
#       if_else(sample_type == "20 hooks", no_skates_hauled * 20,
#         no_skates_hauled * avg_no_hook_per_skate))

# now go ahead and strip those from the main dataset
dat <- filter(dat, !is.na(hooks_observed))
# all good?
stopifnot(sum(is.na(dat$number_observed)) == 0L)

# and join back on our version that has the missing hooks_observed and sample_type columns:
#dat <- bind_rows(dat, never_caught_non_halibut)

# build a df of all possible stations and years for every species
full <- select(
  dat, year, station, station_key, longitude, latitude,
  hooks_observed, hooks_fished, hooks_retrieved,
  avg_no_hook_per_skate, no_skates_hauled, no_skates_set, effective_skates,
  sample_type, usable, soak_time_min, temp_c, depth_m, baits_returned
) |> distinct()
full <- purrr::map_dfr(
  sort(unique(count_dat$species_science_name)),
  \(x) mutate(full, species_science_name = x)
)

dat_with_counts <- filter(dat, !is.na(number_observed))
missing <- anti_join(full, dat_with_counts)
missing$number_observed <- 0L
dat <- bind_rows(dat_with_counts, missing)
dat <- arrange(dat, year, species_common_name, station)

# replace common name with DFO common name:
dat$species_common_name <- NULL
dat <- left_join(dat, select(spp, species_science_name, species_common_name),
  by = join_by(species_science_name)
)
# dat <- filter(dat, usable == "Y")
# dat$usable <- NULL

# plotting helpers ----------------------------------------------------------

coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") |>
  sf::st_crop(xmin = -135, xmax = -121, ymin = 46, ymax = 55.6) |>
  sf::st_transform(crs = 32609)

plot_map <- function(data, colour_column = NULL, species = "north pacific spiny dogfish") {
  data |>
    filter(species_common_name == species) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(crs = 32609) |>
    ggplot() +
    geom_sf(data = coast) +
    geom_sf(pch = 19, size = 0.8, mapping = aes(colour = {{ colour_column }})) +
    facet_wrap(~year) +
    coord_sf(xlim = c(192152, 932873), ylim = c(5357711, 6132290))
}

# checks --------------------------------------------------------------------

# all should be have same number of rows, since zeros have been added:
stopifnot(length(unique(table(dat$species_common_name))) == 1L)

plot_map(dat, hooks_observed) + scale_colour_viridis_c(trans = "sqrt")
plot_map(dat, number_observed/hooks_observed) + scale_colour_viridis_c(trans = "sqrt")
plot_map(dat, depth_m) + scale_colour_viridis_c(trans = "sqrt")

# bring in the 'standard' stations as defined in gfiphc ---------------------

# find 'Standard' grid within gfiphic:
# gfiphc_dat <- readRDS("report/data-cache-2024-05/iphc/north-pacific-spiny-dogfish.rds")$set_counts
# saveRDS(gfiphc_dat, file = "data-raw/gfiphc-dogfish-setcounts.rds")
gfiphc_dat <- readRDS("data-raw/gfiphc-dogfish-setcounts.rds")

pbs_stations <- select(gfiphc_dat, pbs_standard_grid = standard, pbs_usable = usable, station, year) |>
  distinct()
pbs_stations$pbs_standard_grid <- as.character(pbs_stations$pbs_standard_grid)

iphc_stations <- select(dat, year, station, station_key) |>
  mutate(station = as.character(station)) |> distinct()

# find duplicate stations in gfiphc that are not in the IPHC download:
dup_in_gfiphc <- group_by(pbs_stations, year, station) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

dup_in_iphc <- group_by(iphc_stations, year, station) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

dup_in_gfiphc
dup_in_iphc

dups_in_gfiphc_but_not_iphc <- anti_join(select(dup_in_gfiphc, -n), select(dup_in_iphc, -n),
  by = join_by(year, station))

# drop these extra duplicates in 2022 that shouldn't be there!?
pbs_stations <- anti_join(pbs_stations, dups_in_gfiphc_but_not_iphc, by = join_by(station, year))

u_stations <- pbs_stations |> select(station, pbs_standard_grid) |> distinct()
dups <- u_stations |> group_by(station) |> summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

# so, these were chosen as 'standard' in one year but not another:
filter(pbs_stations, station %in% dups$station) |>
  arrange(station, year)

# let's turn them to non-standard since they only appear in 2021:
pbs_stations$pbs_standard_grid[pbs_stations$station %in% dups$station]
pbs_stations$pbs_standard_grid[pbs_stations$station %in% dups$station] <- "N"

# and remove those now duplicated stations:
pbs_stations <- select(pbs_stations, station, pbs_standard_grid) |> distinct()

joined_stations <- left_join(
  iphc_stations,
  pbs_stations,
  by = join_by(station)
)

joined_stations <- joined_stations |>
  mutate(
    pbs_standard_grid = pbs_standard_grid == "Y",
    station = as.integer(station)
  )

# any stations get lost? check:
missing <- anti_join(dat, joined_stations, by = join_by(year, station, station_key))
stopifnot(nrow(missing) == 0L)

dat_pbs <- left_join(dat, joined_stations, by = join_by(year, station, station_key))

# the gfiphc data was up to 2022, from 2023 onwards, need to make some manual decisions:
filter(dat_pbs, year > 2022) |> plot_map(pbs_standard_grid)

# is anything `NA`; if so, fix it! choose TRUE or FALSE for 'standard grid'
# likely FALSE
# look at it:
filter(dat_pbs, species_common_name == "north pacific spiny dogfish") |>
  filter(is.na(pbs_standard_grid)) |>
  select(station, year)

# action happens here:
dat_pbs$pbs_standard_grid[is.na(dat_pbs$pbs_standard_grid)] <- FALSE

# visualize what we have:
plot_map(dat_pbs, pbs_standard_grid)

# manually enter those in inside WCVI waters --------------------------------

filter(dat_pbs, year == 2018) |>
  ggplot() +
  geom_text(aes(x = longitude, y = latitude, label = station), size = 2)
inside_stations <- c(
  2207, 2204, 2203, 2201, 2212, 2211, 2215, 2216, 2219,  2220, 2222, 2223,
  2224, 2225, 2227, 2228, 2229, 2230, 2231, 2234, 2235, 2236, 2238, 2239, 2243,
  2244, 2249, 2259, 2245, 2246
)

dat_pbs <- mutate(dat_pbs, inside_wcvi = station %in% inside_stations)

filter(dat_pbs, year == 2018, !inside_wcvi) |> plot_map(pbs_standard_grid)
filter(dat_pbs, year == 2018, inside_wcvi) |> plot_map(pbs_standard_grid)

dat_pbs |> plot_map(paste(pbs_standard_grid, inside_wcvi))

# bring in pre 1998 data from gfiphc ----------------------------------------
# Need to count all observations per station to get hooks observed for 1995
dat1995 <- left_join(gfiphc::setData1995, gfiphc::countData1995) |> # No data for hook counts in 1995
  tidyr::drop_na(specCount) # there are some stations with no species observed
ho1995 <- dat1995 |>
  group_by(year, station, lat, lon) |>
  summarise(hooksObserved = sum(specCount))
dat1995 <- left_join(dat1995, ho1995) |>
  rename(E_it = "effSkate", catchCount = "specCount") |>
  select(year, station, lon, lat, spNameIPHC, catchCount, hooksObserved, usable, E_it)

dat1996to2002 <- gfiphc::data1996to2002 |>
  filter(year < 1998) |># |> filter(usable == "Y")
  mutate(station = as.character(station))

old <- bind_rows(dat1995, dat1996to2002)
old_br <- filter(old,spNameIPHC == "Hook with Bait") |>
  rename(baits_returned = "catchCount") |>
  select(year, station, lon, lat, baits_returned)
old <- left_join(old, old_br) |>
  mutate(baits_returned = ifelse(is.na(baits_returned), 0, baits_returned))
# add sample type - see Table 2 in Anderson et al. 2022 (Data synopsis 2021)
old$sample_type <- NA
old$sample_type[old$year == 1995] <- "all hooks"
old$sample_type[old$year == 1996] <- "all hooks"
old$sample_type[old$year == 1997] <- "20 hooks"

# check to make sure stations don't clash:
stopifnot(sum(old$station %in% dat_pbs$station) == 0L)

glimpse(dat_pbs)

old <- select(old, year, station, longitude = lon, latitude = lat,
  depth_m = depthAvge, species_common_name = spNameIPHC,
  number_observed = catchCount, hooks_observed = hooksObserved, usable,
  no_skates_hauled = skates, effective_skates = E_it,
  baits_returned)
old <- mutate(old, species_common_name = tolower(species_common_name))

old_sp <- sort(unique(old$species_common_name))
old_sp[!old_sp %in% old$species_common_name]

# most can be dropped, but fix others:
old$species_common_name[old$species_common_name == "spiny dogfish"] <- "north pacific spiny dogfish"
old$species_common_name[old$species_common_name == "sixgill shark"] <- "bluntnose sixgill shark"
old$species_common_name[old$species_common_name == "rock sole"] <- "southern rock sole"
old$species_common_name[old$species_common_name == "rougheye rockfish"] <- "rougheye/blackspotted rockfish complex"
old$species_common_name[old$species_common_name == "sablefish (blackcod)"] <- "sablefish"

# need to pad in zeros to old data:
full_old <- select(
  old, -number_observed, -species_common_name) |>
  distinct()
full_old <- purrr::map_dfr(
  sort(unique(dat_pbs$species_common_name)),
  \(x) mutate(full_old, species_common_name = x)
)
missing <- anti_join(full_old, old)
missing$number_observed <- 0L
old <- bind_rows(old, missing)

# join on the scientific names from PBS:
old <- inner_join(old, select(spp, species_common_name, species_science_name),
  by = join_by(species_common_name))

# make a fake `station_key` to match modern data:
old <- mutate(old,
  station_key = as.character(paste0(year, station)),
  inside_wcvi = FALSE,
  pbs_standard_grid = TRUE # confirmed to match gfiphc decisions
)

dat_all <- bind_rows(
  old,
  dat_pbs |>
    mutate(station_key = as.character(station_key),
           station = as.character(station))) |>
  arrange(year, species_common_name, station)

# fix some known problems!
# - note hooks counted of 1390 in 2018, station 2227
#   looks wrong! likely more like 7 sets * 99 avg. per skate = 693

# confirm:
ho <- filter(dat_all, year == 2018, station == 2227, species_common_name == "north pacific spiny dogfish") |>
  pull(hooks_observed)
stopifnot(ho == 1390)
# fix:
ii <- which(dat_all$year == 2018 & dat_all$station == 2227)
dat_all$hooks_observed[ii] <- dat_all$avg_no_hook_per_skate[ii] * dat_all$no_skates_hauled[ii]

# - also note year = 2004 and station = 2092
#   effective skates only 1.5 but avg. 100 hooks per skate and 1.5
#   skates hauled... so should not be 799 'observed'
#   much more likely that it should be 1.5 * 100 = 150 hooks observed

# confirm:
ho <- filter(dat_all, year == 2004, station == 2092, species_common_name == "north pacific spiny dogfish") |>
  pull(hooks_observed)
stopifnot(ho == 799)
# fix:
ii <- which(dat_all$year == 2004 & dat_all$station == 2092)
dat_all$hooks_observed[ii] <- dat_all$avg_no_hook_per_skate[ii] * dat_all$no_skates_hauled[ii]

# visualize what we have:
plot_map(dat_all, paste(pbs_standard_grid, inside_wcvi)) + scale_colour_brewer(palette = "Dark2")

plot_map(dat_all, number_observed/hooks_observed) + scale_colour_viridis_c(trans = "sqrt")
plot_map(dat_all, temp_c) + scale_colour_viridis_c()

# Filter IPHC species known to be only enumerated explicitly since a certain year:
# See HG predators analysis: gfiphc/vignettes/analysis_for_HG_herring_predators.html
#   Aleutian Skate: Looks like only enumerated explicitly since 2007, 2018 map shows mean 0.31 fish/skate. Include.
#   Abyssal Skate - no catch, ignore.
#   Broad Skate - no catch, ignore.
#   Big Skate - looks like enumerated explicitly since 1998, and 2018 map shows mean 0.69 fish/skate. Include.
#   Roughtail Skate - looks like only caught in three or four years (mean +ve sets 1/177). No catch in 2018. Include.
#   Sandpaper Skate - only shows up for a few years. Mean +ve sets 5/177, 2018 mean 0.14 fish/skate. Include.
#   Longnose Skate - only shows up since 1998 (presumably unidentified beforehand), mean +ve sets 57/135, 2018 mean 0.96 fish/skate. Include.
#   Alaska Skate - only showed up in five years, 2018 mean 0.14 fish/skate. Only plotted since 2003 (like Sandpaper). Include.
dat_all <-
  dat_all |>
  mutate(number_observed = case_when(
    species_common_name == "aleutian skate" & year < 2007 ~ NA,
    species_common_name == "big skate" & year < 1998 ~ NA,
    species_common_name == "longnose skate" & year < 1998 ~ NA,
    species_common_name == "alaska skate" & year < 2003 ~ NA,
    species_common_name == "sandpaper skate" & year < 2003 ~ NA,
    species_common_name == "shortspine thornyhead" & year < 1998 ~ NA, # Not analysed in HG analysis, but is not explicitly identified until 1998 ('unidentified idiots' until then)
    .default = number_observed
  ))

# save it! ------------------------------------------------------------------
# # order nicely:
# iphc <- select(
#   dat_all,
#   year,
#   station,
#   station_key,
#   longitude,
#   latitude,
#   species_common_name,
#   species_science_name,
#   usable,
#   hooks_observed,
#   number_observed,
#   pbs_standard_grid,
#   inside_wcvi,
#   sample_type,
#   depth_m,
#   temp_c,
#   soak_time_min,
#   avg_no_hook_per_skate,
#   no_skates_hauled,
#   no_skates_set,
#   effective_skates
# )

# iphc_sets <- select(dat_all, -species_common_name, -species_science_name, -number_observed) |>
#   distinct()
# iphc_catch <- select(dat_all, station_keyspecies_common_name, species_science_name, number_observed) |>
#   distinct()

iphc_sets <- select(
  dat_all,
  year,
  station,
  station_key,
  longitude,
  latitude,
  usable,
  #hooks_fished,
  hooks_retrieved,
  hooks_observed,
  pbs_standard_grid,
  inside_wcvi,
  sample_type,
  depth_m,
  temp_c,
  soak_time_min,
  avg_no_hook_per_skate,
  no_skates_hauled,
  no_skates_set,
  effective_skates,
  baits_returned
) |>
distinct()

iphc_catch <- select(
  dat_all,
  year,
  station,
  station_key,
  species_common_name,
  species_science_name,
  number_observed
)

attr(iphc_sets, "iphc_download_date") <- DOWNLOAD_DATE
attr(iphc_sets, "data_preparation_date") <- lubridate::today()

attr(iphc_catch, "iphc_download_date") <- DOWNLOAD_DATE
attr(iphc_catch, "data_preparation_date") <- lubridate::today()

usethis::use_data(iphc_sets, overwrite = TRUE)
usethis::use_data(iphc_catch, overwrite = TRUE)
