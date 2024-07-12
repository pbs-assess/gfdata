# Compare IPHC website data against gfiphc

library(dplyr)
library(ggplot2)

load("data/iphc_catch.rda")
load("data/iphc_sets.rda")
gfiphc_dat <- readRDS("data-raw/gfiphc-dogfish-setcounts.rds") |>
  mutate(standard = as.character(standard))

iphc <- inner_join(iphc_catch, iphc_sets, by = join_by(year, station, station_key))

# visual comparison -----------------------------------------

x1 <- filter(gfiphc_dat, usable == "Y", standard == "Y")
x2 <- filter(iphc, species_common_name == "north pacific spiny dogfish", pbs_standard_grid)

xx1 <- filter(x1, year == 2021)
xx2 <- filter(x2, year == 2021)

sum(xx1$N_it20, na.rm = TRUE)
sum(xx2$number_observed)

sum(xx1$E_it20, na.rm = TRUE)
sum(xx2$effective_skates)

hooks_per_effective_skate <- sum(xx2$hooks_observed) / sum(xx1$E_it20)

d1 <- group_by(x1, year) |>
  mutate(E = ifelse(is.na(E_it), E_it20, E_it)) |>
  mutate(N = ifelse(is.na(N_it), N_it20, N_it)) |>
  summarise(n = sum(N, na.rm = TRUE), total_E = sum(E, na.rm = TRUE)) |>
  mutate(type = "gfiphc") |>
  mutate(n_hooks = total_E * hooks_per_effective_skate)
d2 <- group_by(x2, year) |>
  # filter(usable == "Y") |>
  summarise(n = sum(number_observed), n_hooks = sum(hooks_observed)) |>
  mutate(type = "simplified script")

bind_rows(d1, d2) |>
  group_by(type) |>
  filter(year > 1995) |>
  mutate(n_observed = n) |>
  # mutate(n_hooks = n_hooks / n_hooks[year == 2018]) |>
  # mutate(n_hooks = n_hooks / n_hooks[year == 1996]) |>
  tidyr::pivot_longer(cols = c(n_observed, n_hooks), names_to = "measure") |>
  ggplot(aes(year, value, colour = forcats::fct_rev(type))) +
  #geom_line() +
   geom_point() +
  facet_wrap(~measure, scales = "free") +
  geom_vline(xintercept = 1998, lty = 2) +
  geom_hline(yintercept = 2152) +
  labs(colour = "type") +
  ggtitle(unique(x2$species_common_name))

# dig into mismatches on 'usable' -------------------------------------------

glimpse(iphc)
glimpse(gfiphc_dat)
table(iphc$usable)

group_by(gfiphc_dat, year, station) |>
  summarise(n = n()) |> filter(n > 1) |>
  filter(year >= 1998) |>
  as.data.frame()

group_by(iphc, year, station) |>
  filter(species_common_name == "north pacific spiny dogfish") |>
  summarise(n = n()) |> filter(n > 1) |>
  filter(year >= 1998)

# there are a bunch of gfiphc's from 2022 with duplicate rows of NAs!
filter(gfiphc_dat, year == 2022, station == 2295)
filter(iphc, species_common_name == "north pacific spiny dogfish", year == 2022, station == 2306)
filter(gfiphc_dat, year == 2022, station == 2306)
filter(iphc, species_common_name == "north pacific spiny dogfish", year == 2022, station == 2306)

# what about previous years?
group_by(gfiphc_dat, year, station) |>
  summarise(n = n()) |> filter(n > 1) |>
  filter(year >= 1998, year != 2022) |>
  as.data.frame()

# ah, that matches! good
# then remove those extras from 2022:

gfiphc_dat <- filter(gfiphc_dat, !(is.na(N_it20) & is.na(N_it)))

# look better?
group_by(iphc, year, station) |>
  filter(species_common_name == "north pacific spiny dogfish") |>
  summarise(n = n()) |> filter(n > 1) |>
  filter(year >= 1998)
# yep!

gf_st <- gfiphc_dat |> select(year, station, pbs_usable = usable) |> distinct() |>
  filter(year >= 1998)
iphc_st <- iphc |>
  filter(species_common_name == "north pacific spiny dogfish") |>
  select(year, station, iphc_usable = usable) |>
  mutate(station = as.character(station)) |>
  filter(year >= 1998) |>
  distinct()

j <- full_join(gf_st, iphc_st)

# extra in IPHC:
filter(j, is.na(pbs_usable)) |> pull(year) |> table()
# extra in gfiphc:
filter(j, is.na(iphc_usable)) |> pull(year) |> table()

# of those that match, do usables match?

j <- inner_join(gf_st, iphc_st)
filter(j, pbs_usable == "Y", iphc_usable == "N")
filter(j, pbs_usable == "N", iphc_usable == "Y")
filter(j, pbs_usable != iphc_usable) |> as.data.frame()

# what about a full accounting for all diffs?

j <- full_join(gf_st, iphc_st) |> filter(year < 2023)
filter(j, pbs_usable != iphc_usable | is.na(pbs_usable) | is.na(iphc_usable)) |>
  arrange(year, station) |>
  as.data.frame()

# # what about 1997 which should match!? --------------------------------------
#
# xx1 <- filter(gfiphc_dat, year == 1997, usable == "Y")
# sum(xx1$N_it20)
# nrow(xx1)
# mean(xx1$N_it20 > 0)
#
# xx2 <- filter(iphc, year == 1997, species_common_name == "north pacific spiny dogfish")
# sum(xx2$number_observed)
# nrow(xx2)
# mean(xx2$number_observed > 0)
#
# # 2006 looked slightly different
#
# xx1 <- filter(gfiphc_dat, year == 2005) |>
#   select(station, lon, lat, N_it) |> distinct() |>
#   mutate(station = as.numeric(station))
#
# xx2 <- filter(iphc, year == 2005, species_common_name == "north pacific spiny dogfish") |>
#   select(station, longitude, latitude, number_observed) |> distinct()
#
# nrow(xx1)
# nrow(xx2)
#
# anti_join(xx1, xx2)
#

# Compare gfiphc data with output dataset - for all species
# --------------------------------------------------------
library(tidyr)
library(stringr)
iphc <- gfdata::load_iphc_dat()

sample_type_lu <- iphc |> distinct(year, sample_type) |> tidyr::drop_na(sample_type)

raw_d <- readRDS("data-raw/Non-Pacific halibut data.rds") |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year)) |>
  mutate(across(where(is.character), tolower)) |>
  select(-sample_type, -row_number, -setno, -iphc_species_code, -hooks_fished) # these columns have been getting in the way

set <- readRDS("data-raw/Set and Pacific halibut data.rds") |>
  janitor::clean_names()
hal <- set |>
  mutate(scientific_name = "hippoglossus stenolepis",
         species_name = "pacific halibut",
         number_observed = o32_pacific_halibut_count + u32_pacific_halibut_count,
         year = as.numeric(year)) |>
  select(any_of(c(names(raw_d)))) |>
  left_join(x = _,
            y = distinct(raw_d, stlkey, hooks_retrieved, hooks_observed)) # need hook counts from non-halibut data
raw_d <- bind_rows(raw_d, hal) |>
  left_join(sample_type_lu)

if (Sys.info()[["user"]] == "seananderson") {
  dc <- "../gfsynopsis-2022/report/data-cache-nov-2023/"
} else {
  dc <- "../gfsynopsis/report/data-cache-nov-2023/"
}

f <- list.files(paste0(dc, "iphc"), full.names = TRUE)
f <- f[grepl(".rds$", f)]
f <- f[!grepl("iphc/iphc.*.rds", f)]
devtools::load_all('../gfsynopsis')
spp <- gfsynopsis::get_spp_names()
old_d <- purrr::map_dfr(f, \(x) {
  old_d <- readRDS(x)
  if ("set_counts" %in% names(old_d)) {
    old_d <- old_d$set_counts
    old_d$spp_w_hyphens <- stringr::str_extract(x, ".*/(.*).rds$", group = 1)
  }
  old_d |>
    filter(!(year == 2019 & station %in% c("2099", "2107"))) |> # ignore this for now, not worth fixing for this check
    left_join(select(spp, species_common_name, species_science_name, spp_w_hyphens), by = join_by(spp_w_hyphens))
})

old_hook_counts <- readRDS(paste0(dc, "iphc/iphc-hook-counts.rds"))

old2 <- left_join(old_d, old_hook_counts)
old2 <- old2 |>
  rename(hooks_observed = "obsHooksPerSet") |>
  left_join(sample_type_lu) |>
  mutate(number_observed = ifelse(is.na(N_it), N_it20, N_it),
         effective_skates = ifelse(is.na(E_it), E_it20, E_it)) |>
  mutate(species_common_name = ifelse(spp_w_hyphens == 'hook-with-bait', '_hook with bait', species_common_name))

# Filter out a set of station id vectors:
# - then we can do the direct comparison
# - can also look at the check on the 'usable' column

# - it is possible that iphc data online has changed (e.g., usability codes)

common_stations <- filter(iphc, year == 2015) |> pull(station) |> as.character()

combined_df <- bind_rows(
  old2 |>
    mutate(source = 'gfiphc') |>
    filter(!(year == 2019 & station %in% c("2099", "2107"))), #|>,
  iphc |>
    mutate(source = 'raw', station = as.character((station))) |>
    filter(!(year == 2019 & station %in% c("2099", "2107"))),
  distinct(iphc, year, station, baits_returned) |> # compare baited hook counts
    drop_na(baits_returned) |>
    pivot_longer(cols = baits_returned, names_to = "species_common_name", values_to = "number_observed") |>
    mutate(source = 'raw', station = as.character((station))) |>
    mutate(species_common_name = '_hook with bait')
)

# calculate average proportional difference by species
# and absolute difference between counts
# Use inner join to compare stations to
# ok with excluding grenadier, maybe just make a comment in the gfsynopsis
common_stations <- inner_join(
  distinct(iphc, year, station),
  distinct(old2, year, station)
)

not_zero <- combined_df |>
  filter(source == "gfiphc") |>
  filter(standard == "Y") |>
  group_by(species_common_name, year) |>
  summarise(count = sum(number_observed, na.rm = TRUE)) |>
  summarise(all_zero = sum(count) == 0) |>
  filter(!all_zero) |>
  filter(species_common_name != 'pacific grenadier') |> # not included because it is unclear if macrouridae should be coryphaenoides acrolepis
  pull(species_common_name) |>
  gsub("_", "", x = _)
  #filter(usable == "Y") |>

test2 <- combined_df |>
  mutate(species_common_name = gsub("_", "", x = species_common_name)) |>
  filter(species_common_name %in% not_zero) |>
  left_join(x = common_stations, y = _) |>
  group_by(source, species_common_name, year) |>
  summarise(count = sum(number_observed, na.rm = TRUE)) |>
  mutate(species_common_name = factor(species_common_name, levels = c(not_zero[-1], not_zero[1])))

test3 <- test2 |>
  mutate(source2 = ifelse(source == "raw", "IPHC website", source)) |>
  filter(species_common_name != "hook with bait") |>
  mutate(species_common_name = gsub("rougheye/blackspotted rockfish complex", "Rougheye/Blackspotted", species_common_name))

g <- test3 |>
  ggplot(aes(x = year, y = count, colour = source2)) +
  geom_point(data = filter(test3, source == 'gfiphc'), shape = 21, stroke = 1.1, size = 1.1) +
  geom_point(data = filter(test3, source == 'raw'), size = 1.1) +
  facet_wrap(~ stringr::str_to_title(species_common_name), scales = 'free_y', ncol = 6) +
  labs(x = "Year", y = "Count", colour = "Data source") +
  gfplot::theme_pbs() +
  theme(legend.position = c(0.9, 0.05)) +
  # scale_colour_brewer(palette = "Paired")
  scale_colour_manual(values = c("grey40", brewer.pal(6, "Reds")[4]))
dir.create("data-raw/figs", showWarnings = FALSE)
ggsave('data-raw/figs/iphc-compare.pdf', width = 12, height = 15)
ggsave('data-raw/figs/iphc-compare.png', width = 12, height = 15)

old_hc <- old2 |>
    filter(species_common_name == "lingcod") |>
    group_by(year) |>
    summarise(gfiphc_hooks_obs = sum(hooks_observed, na.rm = TRUE))
new_hc <- iphc_sets |>
  group_by(year) |>
  summarise(gfdata_hooks_obs = sum(hooks_observed, na.rm = TRUE))
hc <- left_join(old_hc, new_hc) |>
  tidyr::pivot_longer(cols = c(gfiphc_hooks_obs, gfdata_hooks_obs), names_to = "source")
hc |>
  ggplot(aes(year, value, colour = source)) + geom_line()


OLD <- old2 |>
  filter(species_common_name == "lingcod") |>
  select(year, station, effective_skates, hooks_observed)
OLD |> filter(year %in% c(2008:2014)) |> mutate(hooks_per_skate = hooks_observed / effective_skates) |> ggplot(aes(x = as.factor(year), y = hooks_per_skate)) + geom_violin()

## hook counts look off in 'old' data in 2012!

old_hc <- old2 |>
  filter(species_common_name == "lingcod") |>
  select(year, station, gfiphc_hooks_obs = effective_skates, standard)

hooks_per_skate_avg <- OLD |> filter(year != 2012, year >= 1998) |>
  mutate(r = hooks_observed / effective_skates) |> summarise(m = mean(r, na.rm = T)) |> pull(m)

old_hc$gfiphc_hooks_obs <- old_hc$gfiphc_hooks_obs * hooks_per_skate_avg
new_hc <- iphc_sets |>
  select(year, station, gfdata_hooks_obs = hooks_observed)
hc <- left_join(old_hc, new_hc)
hc |>
  right_join(common_stations) |>
  tidyr::pivot_longer(cols = c(gfiphc_hooks_obs, gfdata_hooks_obs), names_to = "source") |>
  group_by(year, source) |> summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1998) |>
  mutate(source = gsub("gfiphc_hooks_obs", "gfiphc hooks observed", source)) |>
  mutate(source = gsub("gfdata_hooks_obs", "IPHC website hooks observed", source)) |>
  ggplot(aes(year, value, colour = source)) +
    gfplot::theme_pbs() + labs(colour = "Source") +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
    ylab("Hooks observed") + xlab("Year") +
    scale_x_continuous(breaks = seq(1995, 2025, 5)) +
    ylim(0, NA) +
  scale_colour_manual(values = c("grey40", brewer.pal(6, "Reds")[4])) +
  theme(legend.position = "top")
# theme(legend.position = "inside", legend.position.inside = c(0.23, 0.88))
ggsave("data-raw/figs/iphc-hooks-observed-compare.pdf", width = 5, height = 3.1)
ggsave("data-raw/figs/iphc-hooks-observed-compare.png", width = 5, height = 3.1)

# Get proportional difference by species
diff_df <- test2  |>
  pivot_wider(names_from = source, values_from = count) |>
  left_join(sample_type_lu) |>
  filter(!(species_common_name == 'pacific halibut' & sample_type == '20 hooks')) |>
  group_by(species_common_name) |>
  mutate(abs_diff = gfiphc - raw,
         prop_diff = abs_diff / raw) |>
  filter(abs_diff != 0) |>
  arrange(-abs(prop_diff)) |>
  ungroup()
  #arrange(-abs(abs_diff))


# Look at species that never show up in raw FISS data:abs_diff
# Other than the aleutian skate (which we have put as NA before 2007), these
# species are just zero or do not show up at the site-year combinations in the
# raw data

diff_df |>
  filter(raw == 0) |>
  left_join(old2) |>
  filter(gfiphc == N_it | N_it20) |>
  select(year, station) |>
  slice(5) |>
  left_join(raw_d) |>
  arrange(year, station, species_name) |> select(-row_number, -stlkey, -iphc_species_code, -hooks_fished)

diff_df |>
  filter(gfiphc == 0) |>
  left_join(iphc) |>
  filter(raw == number_observed) |>
  mutate(sp = species_common_name) |>
  select(sp, year, station) |>
  slice(4) |>
  left_join(old2) |>
  select(sp, year, station, N_it, N_it20, species_common_name, species_science_name) |>
  arrange(year, station, species_common_name) |>
  print(n = 117)
  #select(-row_number, -stlkey, -iphc_species_code, -hooks_fished, -sample_type) |>
# Basically I don't think we can know why there are these small discrepancies

#
diff_df |>
  arrange(-abs(abs_diff)) |>
  print(n = 100)

# Focus on bigger differences:
bigger_diffs <-
  diff_df |>
    filter(gfiphc > 0 & raw > 0) |>
    filter(!(abs_diff == 1 & prop_diff == 1)) |>
    filter(abs_diff > 5) |>
    arrange(-abs(prop_diff))

bigger_diffs |> print(n = 58)

bigger_diffs |> filter(year != 2006) |> print(n = 46)

combined_df |>
  left_join(x = common_stations, y = _) |>
  select(year, station, species_common_name, number_observed,
         source, species_science_name) |>
  # filter(species_common_name == "shortspine thornyhead" & year > 1998) |> # difference is likely due to differences in what was considered 'unidentified thornyhead'
  # filter(species_common_name == "redbanded rockfish", year == 2006) |> # big differences due to likely change in hooks_observed
  # filter(species_common_name == "sablefish", year != 2006) |> # other than the 2006 differences, some small changes I can't explain
  # filter(species_common_name == "quillback rockfish", year == 2019) |> # sum of small differences at 5 sites
  filter(species_common_name == "arrowtooth flounder", year != 2006) |>
  pivot_wider(names_from = source, values_from = number_observed) |>
  mutate(diff = gfiphc - raw) |>
  filter(diff > 0) |>
  arrange(-diff)

# Looking into thornyheads ---
# Differences are due to differences in what is considered unidentified thornyhead
# Pooling them all (similar to what we did for RE/BS) just creates larger discrepancies
# with higher counts in the raw FISS data relative to gfiphc, so I have left them
# as is. Also see: https://github.com/pbs-assess/gfiphc/issues/18
filter(raw_d, stringr::str_detect(scientific_name, "sebastolobus")) |>
  group_by(year, station) |>
  summarise(rows = n()) |>
   filter(rows > 1) |>
   left_join(raw_d) |>
   arrange(scientific_name) |>
   print(n = 60)
# ---

# Looking into redbanded ---
filter(old2, year == 2006, station == 2088) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0) # unclear why there is this difference
filter(raw_d, year == 2006, station == 2088) |> filter(number_observed > 0)
# I think this is a case where IPHC changed what hooks were considered observed
# and so the raw dataset has changed for undescribed reasons
# Seems like a similar case for year == 2006, station %in% c(2108, 2086, 2167); hooks_observed has changed by 100
# Side note - I think this likely also explains the large discrepancies I found
# last year in the number of hooks_observed in the gfiphc data and the IPHC FISS
# data
# ---

# Looking into sablefish ---
filter(old2, year == 2006, station == 2105) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0)
filter(raw_d, year == 2006, station == 2105)
# In 2006 I think this is the same thing with the change in hooks_observed

filter(old2, year == 2011, station == 2018) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0)
filter(raw_d, year == 2011, station == 2018)
# small difference, unclear why

filter(old2, year == 2010, station == 2017) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0)
filter(raw_d, year == 2010, station == 2017)
# small difference, unclear why
# ---

# Looking into quillback ---
filter(old2, year == 2019, station == 2130) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0)
filter(raw_d, year == 2019, station == 2130)
# small differences

# Looking into arrowtooth ---
filter(old2, year == 2011, station == 2159) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(number_observed > 0) #|>
  # mutate(all_obs = sum(number_observed)) # doesn't include all observed species, so this number is less than hooks_observed

filter(raw_d, year == 2011, station == 2159) |>
  select(-stlkey) |>
  mutate(all_obs = sum(number_observed))
# ------------------------------------------------------------------------------

# There are just some discrepancies in the raw IPHC FISS data (total number_observed per set != hooks_observed)
raw_d |>
  group_by(year, station, stlkey, sample_type, hooks_observed) |>
  summarise(all_obs = sum(number_observed), .groups = "drop") |>
  filter(sample_type == "all hooks") |>
  mutate(ho_diff = all_obs - hooks_observed) |>
  arrange(-abs(ho_diff)) |>
  filter(year != 2012) |>
  print(n = 50)

filter(raw_d, stlkey == 20161353)
filter(raw_d, stlkey == 20060139)

# Notes:
# -------------
# Maybe we should grab the halibut data from GFBio for 2012 to be able to get the chum
# only baited hooks because otherwise the halibut counts will be affected by
# catch-rate effects of alternative baits - maybe for our purposes this is fine?
# Also for the 2012 data, I think they give the halibut counts for all fished hooks
# regardless of bait, and so when you calculate total number_observed (and include
# halibut) this value can be than hooks_observed. So I have noted this in the docs
# -------------
check_2012 <- raw_d |>
  group_by(year, station, stlkey, sample_type, hooks_observed) |>
  summarise(all_obs = sum(number_observed), .groups = "drop") |>
  filter(sample_type == "all hooks") |>
  mutate(ho_diff = all_obs - hooks_observed) |>
  arrange(-abs(ho_diff)) |>
  filter(year == 2012)

check_2012 |>
  select(year, station) |>
  left_join(old2) |>
  filter(number_observed > 0) |>
  select(year, station, species_science_name, species_common_name, N_it, N_it20, number_observed, hooks_observed) |>
  filter(species_common_name == "pacific halibut")

check_2012 |>
  select(year, station) |>
  left_join(raw_d) |>
  filter(number_observed > 0) |>
  filter(species_name == "pacific halibut")


test <- load_iphc_dat()

test |>
  filter(year == 2012) |> glimpse()
  # filter(station_key == 20120162) |>
  # filter(number_observed > 0) |>
  # glimpse()
