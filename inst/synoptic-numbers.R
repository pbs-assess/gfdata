# x <- readRDS("/Volumes/Extreme-SSD/src/gfsynopsis-2021/report/data-cache/north-pacific-spiny-dogfish.rds")
# sets <- x$survey_sets
# samps <- x$survey_samples
# data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
# data_survey_samples

data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
data_surveysets

sets <- data_surveysets
samps <- data_survey_samples

library(tidyverse)

sets <- filter(sets, survey_abbrev == "SYN QCS")
samps <- filter(samps, survey_abbrev == "SYN QCS")

glimpse(samps)
glimpse(sets)
test <- readRDS("output/predicted_weight_trawl.rds")
glimpse(test)

y <- left_join(sets, select(test, year, survey_abbrev, survey_id, species_code, sample_id, length, weight, specimen_id, weight_predicted))
glimpse(y)

# plot(sets$catch_weight, sets$catch_count)
#
# filter(sets, catch_count > 10) %>% glimpse() #how many sets have catch counts greater than 10? most have zero catch counts
#
# filter(y, sample_id == 233645) %>% glimpse
#
# filter(y, is.na(sample_id)) %>% glimpse #any rows without a fishing_id or sample id
#
# group_by(y, sample_id) %>%
#   summarise(n = length(unique(specimen_id))) #number of samples per sample id (fishing id)
#
# filter(y, !is.na(weight))
# filter(y, is.na(weight)) #weight of individual samples
#
# filter(samps, !is.na(weight))
# filter(samps, is.na(weight))
#
# #x <- filter(y, !is.na(sample_id)) %>% tail %>% glimpse
#
# #x <- filter(y, fishing_event_id == 5491292)



#NEST THIS IN THE FUNCTION??
# #predict length weight relationships as not all trawls have weight information (but do have length)
# femaletw <- fit_length_weight(
#   x,
#   sex = ("female"),
#   downsample = Inf,
#   min_samples = 50L,
#   method = c("tmb"), # method = c("tmb", "rlm", "lm"),
#   # df = 3,
#   # too_high_quantile = 1,
#   usability_codes = NULL,
#   scale_weight = 1 / 1000 # grams to kgs
# )
#
# maletw <- fit_length_weight(
#   x,
#   sex = ("male"),
#   # sex = c("female", "male", "all"),
#   downsample = Inf,
#   min_samples = 50L,
#   method = c("tmb"), # method = c("tmb", "rlm", "lm"),
#   # df = 3,
#   # too_high_quantile = 1,
#   usability_codes = NULL,
#   scale_weight = 1 / 1000
# )
#
#
# trawl_f <- filter(x, sex == 2)
# trawl_f$weight_predicted <- exp(femaletw$pars$log_a +
#                                   femaletw$pars$b * log(trawl_f$length)) * 1000
#
# trawl_m <- filter(x, sex == 1)
# trawl_m$weight_predicted <- exp(maletw$pars$log_a +
#                                   maletw$pars$b * log(trawl_m$length)) * 1000
#
#
# predicted_weight_tw <- rbind(trawl_m, trawl_f)
# predicted_weight_tw2 <- select(predicted_weight_tw, year, survey_abbrev, survey_id, species_code, sample_id, length, weight, specimen_id, weight_predicted)
# test <- readRDS("output/predicted_weight_trawl.rds")
#



# need a function with logic for an individual fishing event:
get_nfish <- function(x, too_small = 0.1, too_big = 12) { #too small amd too big are min and max weights in kg
  #check for no catch counts or catch weights
  stopifnot(length(unique(x$catch_count)) == 1L) #stop if catch count is not 1?
  stopifnot(length(unique(x$catch_weight)) == 1L)

  #sum up sampe weight, samp count, and catch weight for each tow
  tot_samp_weight <- sum(x$weight, na.rm = TRUE) / 1000 #grams to kg
  tot_samp_weight_predicted <- sum(x$weight_predicted, na.rm = TRUE) / 1000 #grams to kg
  tot_samp_count <-  x %>% tally()#number of individuals
  catch_weight <- x$catch_weight[1] #catch weight of tow

  #check
  if (x$catch_count > 1 || is.na(tot_samp_weight) || tot_samp_weight == 0) { # no samples, check if set count looks reasonable by estimating mean weight of fish:
    implied_weight_per_fish <- catch_weight / x$catch_count
    if (implied_weight_per_fish < too_small || implied_weight_per_fish > too_big) #check mean weight of fish
      stop("Implied fish weight using set data is too small or big. Will calculate count using sample data", call. = FALSE) #meaning something is up with the set weight and set count, count then ignore these columns and estimate based on samples.
    est_catch_count <- x$catch_count[1]
    # } else {
    ratio <- tot_samp_weight / catch_weight #is samp_weight reasonable, test against catch_weight
    if (ratio < 0.95 || ratio > 1.05)
      stop("total sample weight / catch weight is beyond the stated tolerance. These observations shoudl be removed???", call. = FALSE) #should we remove these ones?
    #n_fish_samp <- nrow(x) or replace with this from above tot_samp_count <-  x %>% tally()#number of individuals
  } else {
    if(is.na(tot_samp_weight) == TRUE)
      print("There are NAs in sample weights. Will use predicted weights to calculate mean weight of samples.", call. = FALSE)
    implied_weight_per_fish <-  tot_samp_weight/tot_samp_count
    est_catch_count <- catch_weight/ implied_weight_per_fish
    # } else {
    if(!is.na(tot_samp_weight) == TRUE)
      print("Using sample weights to calculate mean weight of samples.")
    implied_weight_per_fish <-  tot_samp_weight/tot_samp_count
    est_catch_count <- catch_weight/ implied_weight_per_fish
  }
  est_catch_count
}





z <- filter(y, fishing_event_id == 5491292)
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

