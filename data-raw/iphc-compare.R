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
sum(xx2$number_observed)

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
  geom_line() +
  # geom_point() +
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
