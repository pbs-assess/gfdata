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
iphc <- load_iphc_dat()

f <- list.files("../gfsynopsis/report/data-cache-nov-2023/iphc", full.names = TRUE)
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
    left_join(select(spp, species_common_name, species_science_name, spp_w_hyphens))
})
old_hook_counts <- readRDS("../gfsynopsis/report/data-cache-nov-2023/iphc/iphc-hook-counts.rds")

sample_type_lu <- iphc |> distinct(year, sample_type) |> tidyr::drop_na(sample_type)

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

test_stations <- filter(iphc, year == 2015) |> pull(station) |> as.character()

test <- bind_rows(
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
not_zero <- test |>
  filter(source == "gfiphc") |>
  filter(standard == "Y") |>
  group_by(species_common_name, year) |>
  summarise(count = sum(number_observed, na.rm = TRUE)) |>
  summarise(all_zero = sum(count) == 0) |>
  filter(!all_zero) |>
  pull(species_common_name)
  #filter(usable == "Y") |>

test2 <- test |>
filter(species_common_name %in% not_zero) |>
  #filter(station %in% test_stations) |>
  group_by(source, species_common_name, year) |>
  summarise(count = sum(number_observed, na.rm = TRUE))
ggplot(data = test2, aes(x = year, y = count, colour = source)) +
  geom_point(data = filter(test2, source == 'gfiphc'), shape = 21, stroke = 1.2) +
  geom_point(data = filter(test2, source == 'raw')) +
  facet_wrap(~ species_common_name, scales = 'free_y')

# Some data are in the gfiphc/GFBio data but not int he current raw FISS data
t2 <- filter(old2, species_common_name == "darkblotched rockfish", N_it > 0) |>
  select(species_science_name, year, station, number_observed)

left_join(t2, filter(test, source == "raw")) |> glimpse()


t1 <- filter(test, species_common_name == "north pacific spiny dogfish") |>
  #filter(effective_skates == 0) |>
  filter(source == "gfiphc", year == 1996)

t2 <- filter(test, species_common_name == "north pacific spiny dogfish") |>
  filter(effective_skates == 0)


hooks_per_effective_skate <- sum(xx2$hooks_observed) / sum(xx1$E_it20)

t2 |> select(year, station, species_common_name:no_skates_set) |> glimpse()

temp <- left_join(
t2 |> select(year, station, species_common_name:no_skates_set) |>
mutate(check_eff_skate = hooks_observed / avg_no_hook_per_skate),
test |> filter(source == "gfiphc")
)

glimpse(temp)

