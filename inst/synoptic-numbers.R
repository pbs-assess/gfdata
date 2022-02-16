
library(tidyverse)
library(gfdata)
library(gfplot)
library(here)
library(testthat)

#x <- readRDS("/Volumes/Extreme-SSD/src/gfsynopsis-2021/report/data-cache/north-pacific-spiny-dogfish.rds")

data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
data_survey_samples

data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
data_surveysets

sets <- data_surveysets
samps <- data_survey_samples

#sets <- filter(sets, survey_abbrev == "SYN QCS")
#samps <- filter(samps, survey_abbrev == "SYN QCS")

sets <- filter(sets, survey_abbrev == "SYN WCVI")
samps <- filter(samps, survey_abbrev == "SYN WCVI")

samps <- filter(samps, !is.na(species_common_name) == TRUE)
glimpse(samps)
glimpse(sets)

test2<- filter(samps, fishing_event_id == 1925746)
test3 <- filter (samps, fishing_event_id == 1925746 )
sum(test2$weight)/1000
sum(test3$weight_predicted)
unique(samps$species_common_name)

###################################################################################
#calculate length weight relationship to have predicted weights in the database
###################################################################################
#predict length weight relationships as not all trawls have weight information (but do have length)
femaletw <- fit_length_weight(
  samps,
  sex = ("female"),
  downsample = Inf,
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  # df = 3,
  # too_high_quantile = 1,
  usability_codes = NULL,
  scale_weight = 1 / 1000 # grams to kgs
)

maletw <- fit_length_weight(
  samps,
  sex = ("male"),
  # sex = c("female", "male", "all"),
  downsample = Inf,
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  # df = 3,
  # too_high_quantile = 1,
  usability_codes = NULL,
  scale_weight = 1 / 1000

)


trawl_f <- filter(samps, sex == 2)
trawl_f$weight_predicted <- exp(femaletw$pars$log_a +
                                  femaletw$pars$b * log(trawl_f$length)) * 1000

trawl_m <- filter(samps, sex == 1)
trawl_m$weight_predicted <- exp(maletw$pars$log_a +
                                  maletw$pars$b * log(trawl_m$length)) * 1000


predicted_weight_tw <- rbind(trawl_m, trawl_f)
predicted_weight_tw2 <- select(predicted_weight_tw, year, survey_abbrev, survey_id, species_code, sample_id, length, weight, specimen_id, weight_predicted)
test <- saveRDS(predicted_weight_tw2, "output/predicted_weight_trawl.rds")
test <- readRDS("output/predicted_weight_trawl.rds")
glimpse(test)
#make a column that has the raw weight data and then includes the predicted weight data if nas.
test$weight_complete <- ifelse(!is.na(test$weight) == TRUE, test$weight, test$weight_predicted)
test$weight_complete

#join set and samples with predicted weight
y <- left_join(sets, select(test, year, survey_abbrev, survey_id, species_code, sample_id, length, weight_complete, specimen_id))
glimpse(y)


###########################################################################
#function that will check if the weight calculatiosn make sense
###########################################################################
check_implied_weight <- function(implied_weight_per_fish, too_small, too_big) {
    if (implied_weight_per_fish < too_small || implied_weight_per_fish > too_big) {
      stop("Implied fish weight using set data is too small or big. ",
        call. = FALSE
      )
    }
}


###########################################################################
#function that convert the weight to counts
###########################################################################
# need a function with logic for an individual fishing event:
# too small amd too big are min and max weights in kg
get_nfish <- function(x, too_small = 0.04, too_big = 12) {

  # cat(unique(x$fishing_event_id), "\n")
  # check for no catch counts or catch weights
  stopifnot(length(unique(x$catch_count)) == 1L)
  stopifnot(length(unique(x$catch_weight)) == 1L)
  if (any(is.na(x$weight_complete))) {
    stop("Some `weight`s are NA. Run length weight relationship or double check data", call. = FALSE)
  }

  # 1. check if no dogfish:
  if (x$catch_count[1] == 0 && all(is.na(x$sample_id))) {
    tibble(count = 0, fishing_event_id = x$fishing_event_id[1], implied_weight_per_fish = NA, method = "catch count")
  }

  # 2. check if we can just use `catch_count`:
  # sum up sample weight, samp count, and catch weight for each tow:
  tot_samp_weight <- sum(x$weight_complete, na.rm = TRUE) / 1000 # grams to kg
  tot_samp_count <- nrow(x) # number of individuals
  catch_weight <- x$catch_weight[1] # catch weight of tow
  catch_count <- x$catch_count[1]

  # 3. extrapolate sample catch weight to full catch:
  # no samples, check if set count looks reasonable by estimating mean weight of fish:
  if (x$catch_count >= 1 || is.na(tot_samp_weight) || tot_samp_weight == 0) {
    implied_weight_per_fish <- catch_weight / catch_count
    check_implied_weight(implied_weight_per_fish, too_small, too_big)
    est_catch_count <- catch_count
    method <- "catch count"
  } else {
    # is tot_samp_weight reasonable? test against catch_weight:
    if (tot_samp_weight / catch_weight > 1.1) {
      stop("total sample weight / catch weight is too big. ",
        "Check this fishing_event_id",
        call. = FALSE
      )
    } # sample weight looks plausible, proceed
    implied_weight_per_fish <- tot_samp_weight / tot_samp_count
    check_implied_weight(implied_weight_per_fish, too_small, too_big)
    est_catch_count <- catch_weight / implied_weight_per_fish
    method <- "sample weight extrapolation"
  }
  tibble(count = est_catch_count, fishing_event_id = x$fishing_event_id[1],
    implied_weight_per_fish = implied_weight_per_fish, method = method)
}


unique(fishing_event_id)
z <- filter(y, fishing_event_id == 5491292)
expect_equal(round(get_nfish(z), 3), 8.135)

z <- filter(y, fishing_event_id == 308687)
expect_equal(get_nfish(z), 0)

z <- filter(y, fishing_event_id == 308681)
expect_equal(get_nfish(z), z$catch_count)

z <- filter(y, fishing_event_id == 308673)
expect_error(get_nfish(z), regexp = "weight")

yy <- group_by(y, fishing_event_id) %>%
  mutate(any_weight_na = any(is.na(weight_complete))) %>%
  filter(!any_weight_na)

counts <- yy %>%
  group_by(fishing_event_id) %>%
  group_split() %>%
  purrr::map_dfr(get_nfish)

counts %>% filter(count > 1000)
counts %>% filter(count > 500)

# problem:
filter(y, fishing_event_id == 501905) %>% get_nfish()
filter(y, fishing_event_id == 5099854) %>% get_nfish()
filter(y, fishing_event_id == 1925756) %>% get_nfish()
filter(y, fishing_event_id == 3234496) %>% get_nfish()
filter(y, fishing_event_id == 3234495) %>% get_nfish()

unname(quantile(counts$count, probs = 0.9)) * 10

counts2 <- filter(counts, count < 500)

yy2 <- right_join(yy, rename(counts2, calculated_count = count))

ggplot(yy2, aes(calculated_count, catch_weight)) + geom_point()


filter(y, fishing_event_id == 5491275) %>% get_nfish()

filter(y, fishing_event_id == 1925746) %>% get_nfish()

yy_counts <- group_by(yy, fishing_event_id) %>%
  summarise(count = catch_count[1])
plot(yy_counts$count, counts)


z <- filter(y, fishing_event_id == 5491292)
sum(z$weight/1000)
b <- z$catch_weight[1]
xmean <- ((sum(z$weight))/1000)/(length(unique(z$specimen_id)))
count = b/xmean
get_nfish(z)

z <- filter(y, fishing_event_id == 4363101) #this ex. highlights a reported catch weight that is 1, so ignore catch count and go with the estimated catch count.
b <- z$catch_weight[1]
#count is one
xmean <- ((sum(z$weight))/1000)/(length(unique(z$specimen_id)))
count = b/xmean
get_nfish(z)

z <- filter(y, fishing_event_id == 1925746) #this example highlights that we need the predicted weight as there are sample lengths but not sample weight for this fishing even.
b <- z$catch_weight[1] #sample weight is greater than catch weight??
sum(z$weight_predicted)/1000
#count is one
xmean <- ((sum(z$weight))/1000)/(length(unique(z$specimen_id)))
count = b/xmean
get_nfish(z)

z <- filter(y, fishing_event_id == 308772) #this example highlights that we need the predicted weight as there are sample lengths but not sample weight for this fishing even.
b <- z$catch_weight[1] #sample weight is greater than catch weight??
sum(z$weight_predicted)/1000
#count is one
xmean <- ((sum(z$weight_predicted))/1000)/(length(unique(z$specimen_id)))
count = b/xmean
get_nfish(z)


z <- dplyr::filter(y, fishing_event_id == 1925746)

# Run all -------------------------------------------------------------

if (Sys.info()[["user"]] == "seananderson") {
  x <- readRDS("/Volumes/Extreme-SSD/src/gfsynopsis-2021/report/data-cache/north-pacific-spiny-dogfish.rds")
  sets <- x$survey_sets
  samps <- x$survey_samples
} else {
  data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
  data_survey_samples

  data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
  data_surveysets

  sets <- data_surveysets
  samps <- data_survey_samples
}

sets <- filter(sets, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG"))
samps <- filter(samps, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG"))

y <- left_join(sets, select(samps, year, survey_abbrev, survey_id, species_code, sample_id, length, weight, specimen_id))

yy <- group_by(y, fishing_event_id) %>%
  mutate(any_weight_na = any(is.na(weight))) %>%
  filter(!any_weight_na)

counts <- yy %>%
  group_by(survey_abbrev, fishing_event_id) %>%
  group_split() %>%
  purrr::map_dfr(get_nfish)

hist(counts$count)
hist(counts$implied_weight_per_fish)

yy2 <- right_join(yy, rename(counts, calculated_count = count))

ggplot(yy2, aes(calculated_count, catch_weight, colour = method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~survey_abbrev)

ggplot(yy2, aes(calculated_count, catch_weight, colour = implied_weight_per_fish)) +
  geom_point() +
  scale_color_viridis_c(trans = "log10") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~survey_abbrev)

filter(y, fishing_event_id == 4356032) %>% View()


