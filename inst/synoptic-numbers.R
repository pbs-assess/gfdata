
library(tidyverse)
library(gfdata)
library(gfplot)
library(here)
library(testthat)

#############################
# Load data
#############################

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
samps <- filter(samps, !is.na(species_common_name) == TRUE)



###################################################################################
# function to calculate length weight relationship to have predicted weights in the database
###################################################################################
# predict length weight relationships as not all trawls have weight information (but do have length)

# assumptions male/female are coded as 1 and 2 respectively
# assumptions weight is collected in g
# assumes that samples with no sex information (coded as 0) are males.


predict_weight <- function(xx) {

  # 1. check if this is necessary
  if (!anyNA(xx$weight)) {
    stop("All weights are included in your dataset, no need to predict weights.", call. = FALSE)
  }

  # 2 . check sex codes are correct
  x <- c(0, 1, 2)
  if (!any(unique(xx$sex) %in% x)) {
    stop("codes do not match 0,1,2", call. = FALSE)
  }

  # 3. convert 0 to males
  x <- 0
  if (any(unique(xx$sex) %in% x)) {
    print(paste(nrow(filter(xx, sex == 0)), "samples are missing sex information (coded as 0), the predicted weight for these individuals will be the average of male and female predicted weights."))
    xy <- filter(xx, sex == 0 | sex == 1)
    xy$sex <- replace(xy$sex, xy$sex == 0, 1) # males are 1
    maletw <- fit_length_weight(
      xy,
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

    trawl_m <- filter(xx, sex == 0)
    trawl_m$weight_predicted_m <- exp(maletw$pars$log_a +
      maletw$pars$b * log(trawl_m$length)) * 1000

    xxx <- filter(xx, sex == 2 | sex == 0)
    xxx$sex <- replace(xxx$sex, xxx$sex == 0, 2) # females are 2
    femaletw <- fit_length_weight(
      xxx,
      sex = ("female"),
      downsample = Inf,
      min_samples = 50L,
      method = c("tmb"), # method = c("tmb", "rlm", "lm"),
      # df = 3,
      # too_high_quantile = 1,
      usability_codes = NULL,
      scale_weight = 1 / 1000 # grams to kgs
    )

    trawl_f <- filter(xx, sex == 0)
    trawl_f$weight_predicted_f <- exp(femaletw$pars$log_a +
      femaletw$pars$b * log(trawl_f$length)) * 1000

    predicted_weight_tw <- inner_join(trawl_m, trawl_f)
    predicted_weight_tw <- predicted_weight_tw %>% mutate(weight_predicted = rowSums(across(weight_predicted_f:weight_predicted_f)) / 2)
    predicted_weight_nosex <- select(predicted_weight_tw, year, survey_abbrev, fishing_event_id, survey_id, species_code, sample_id, sex, length, weight, specimen_id, weight_predicted)
  }

  # check if there are codes for male and female, if only one sex then run one, otherwise run both.
  x <- c(1, 2)
  if (any(unique(xx$sex) %in% x)) {
    # 2. run fit_length_weight for males and females separately.
    femaletw <- fit_length_weight(
      xx,
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
      xx,
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

    trawl_f <- filter(xx, sex == 2)
    trawl_f$weight_predicted <- exp(femaletw$pars$log_a +
      femaletw$pars$b * log(trawl_f$length)) * 1000

    trawl_m <- filter(xx, sex == 1)
    trawl_m$weight_predicted <- exp(maletw$pars$log_a +
      maletw$pars$b * log(trawl_m$length)) * 1000


    predicted_weight_tw <- rbind(trawl_m, trawl_f)
    predicted_weight_tw2 <- select(predicted_weight_tw, year, survey_abbrev, fishing_event_id, survey_id, species_code, sample_id, sex, length, weight, specimen_id, weight_predicted)
  }
  predicted_weight_tw3 <- rbind(predicted_weight_nosex, predicted_weight_tw2)
  return(predicted_weight_tw3)
}



###########################################################################
# function that will check if the weight calculations make sense
###########################################################################
check_implied_weight <- function(implied_weight_per_fish, too_small, too_big) {
  if (implied_weight_per_fish < too_small || implied_weight_per_fish > too_big) {
    warning("Implied fish weight using set data is too small or big. ",
      call. = FALSE
    )
  }
}


###########################################################################
# function that convert the weight to counts
###########################################################################
# need a function with logic for an individual fishing event:
# too small amd too big are min and max weights in kg
get_nfish <- function(x, too_small = 0.04, too_big = 12) {

  # cat(unique(x$fishing_event_id), "\n")
  # check for no catch counts or catch weights
  stopifnot(length(unique(x$catch_count)) == 1L)
  stopifnot(length(unique(x$catch_weight)) == 1L)
  if (any(is.na(x$weight_complete))) {
    stop("Some `weight`s are NA. Run length weight relationship, or some fishing ids don't have samples, or double check data", call. = FALSE)
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
      warning("total sample weight / catch weight is too big.")
      print(paste("Check fishing_event_id number:", as.character(x$fishing_event_id)))
      est_catch_count <- NA
      method <- "total sample weight / catch weight is too big"
    } # sample weight looks plausible, proceed
    implied_weight_per_fish <- tot_samp_weight / tot_samp_count
    check_implied_weight(implied_weight_per_fish, too_small, too_big)
    est_catch_count <- catch_weight / implied_weight_per_fish
    method <- "sample weight extrapolation"
  }
  tibble(
    count = est_catch_count, fishing_event_id = x$fishing_event_id[1],
    implied_weight_per_fish = implied_weight_per_fish, method = method
  )
}



# Run all -------------------------------------------------------------
##############################################################################################
# CODE TO RUN ABOVE FUNCTIONS ON THE SAMPLE DATABASE
##############################################################################################

# predict weights
samps_weightcomplete <- predict_weight(samps)

# create a weight column that has either predicted and collected weights
samps_weightcomplete$weight_complete <- ifelse(!is.na(samps_weightcomplete$weight) == TRUE, samps_weightcomplete$weight, samps_weightcomplete$weight_predicted)

# join set and samples with predicted weight
y2 <- left_join(sets, select(samps_weightcomplete, year, survey_abbrev, survey_id, species_code, sample_id, length, weight_complete, specimen_id, fishing_event_id),
  by = c("year" = "year", "survey_abbrev" = "survey_abbrev", "fishing_event_id" = "fishing_event_id")
)
dim(y2)

yy <- group_by(y2, survey_abbrev, fishing_event_id) %>%
  mutate(any_weight_na = any(is.na(weight_complete))) %>%
  filter(!any_weight_na)
length(unique(y2$fishing_event_id))
length(unique(yy$fishing_event_id))

## fishing events with no samples
yy_nosamples <- group_by(y2, survey_abbrev, fishing_event_id) %>%
  mutate(any_weight_na = any(is.na(weight_complete))) %>%
  filter(any_weight_na)
yy_nosamples2 <- yy_nosamples %>% distinct()
length(unique(yy_nosamples$fishing_event_id))

# run weights to counts conversion function
counts <- yy %>%
  group_by(fishing_event_id) %>%
  group_split() %>%
  purrr::map_dfr(get_nfish)
unique(counts$method)
filter(counts, fishing_event_id == 481885)

# problems
# look at some of the entires that had warnings
ch <- filter(y2, fishing_event_id == 2179105)
x <- filter(y, fishing_event_id == 1281290)
sum(ch$weight_complete) / 1000
ch$catch_weight


# look at distribution of counts
unname(quantile(counts$count, probs = 0.9)) * 10 # upper quantile number of counts if 5,440 fish
highcounts <- counts %>% filter(count > 1000)

highcounts <- counts %>% filter(count > 500)
highcount_fi <- filter(y2, fishing_event_id == 501905) # QSC syn has a catch_weight of 12465.4 kg and a count of 13497.5669 fish
hist(counts$count)
# fishing_event_id == 3233304 has an implied weight of 0.0008913684 per fish and counts of 4,150 fish
# fishing_event_id == 1281273 has an implied weight of 1.1117957325 per fish and counts of 9,941.5744 fish
# fishing_event_id == 4546331 has an implied weight of 1.0810000000 per fish and counts of 8,978.0759 fish

counts2 <- filter(counts, count < 500)
hist(counts2$implied_weight_per_fish)

yy2 <- right_join(yy, rename(counts2, calculated_count = count))
ggplot(yy2, aes(calculated_count, catch_weight)) +
  geom_point()

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


#############################
# end of run
##############################
below_3sd_catchweight <- function(z) {
  x <- log10(z$catch_weight)
  x[is.infinite(x)] <- NA
  x1 <- exp(sd(x, na.rm = TRUE))
  mean <- mean(x, na.rm = TRUE)
  z$within_3sd <- abs(exp(log10(z$catch_weight) - mean)) < 3 * x1
  return(z)
}

below_3sd <- function(z) {
  x <- exp(sd(log(z$weight_complete), na.rm = TRUE))
  z$within_3sd <- abs(exp(log(z$weight_complete) - mean(log(z$weight_complete), na.rm = TRUE))) < 3 * x
  return(z)
}


### write a function that completes checks on the database
y2 # has sets with no sampes

x <- set_sample_check(y2)

set_sample_check <- function(y2) {

  stopifnot(length(unique(y2$species_common_name)) == 1L)
  # stopifnot(length(unique(x$catch_weight)) == 1L)

  # 1. check for sets with no samples
  nosamps <- y2 %>%
    group_by(survey_abbrev, fishing_event_id) %>%
    summarize(na_in_sample_id = sum(sample_id)) %>%
    filter(is.na(na_in_sample_id) == TRUE)
  tibble(fishing_event_id = nosamps$fishing_event_id, report = "no samples for this fishing event id")

  # 2. check for sets where sum sample weight is greater than catch_weight
  #  ##### multivariate outliers catch weight and sum of weight of samples
  # calculate where sum samples are > weight_complete
  # plot of samp weights
  higher_sampsweight <- y2 %>%
    group_by(fishing_event_id, catch_weight) %>%
    summarize(sample_sum = sum(weight_complete) / 1000)

  higher_sampsweight2 <- higher_sampsweight %>% filter(sample_sum > catch_weight) %>% distinct(fishing_event_id)

  df <- tibble(fishing_event_id = higher_sampsweight2$fishing_event_id, report = "sample weight is > catch weight")

  plot_sample_bigger_catch <-
    ggplot(y3, aes(catch_weight, sample_sum), colour = "grey50") +
    geom_point() +
    scale_color_viridis_c(trans = "log10") # +
  # geom_text(aes(label = as.character(fishing_event_id)))
  diag <- plot_sample_bigger_catch +
    geom_point(data = y4, mapping = aes(catch_weight, sample_sum), col = "red") +
    scale_x_log10() +
    scale_y_log10()


  # create new column in data frame to hold Mahalanobis distances
  multi_outliers <- higher_sampsweight %>%
    select(catch_weight, sample_sum, fishing_event_id) %>%
    drop_na() %>%
    filter(catch_weight > 0 & sample_sum > 0) %>%
    mutate(
      logcatch_weight = log10(catch_weight),
      logsample_sum = log10(sample_sum)
    ) %>%
    select(logcatch_weight, logsample_sum, fishing_event_id)


  multi_outliers$mahal <- mahalanobis(multi_outliers, colMeans(multi_outliers, na.rm = TRUE), cov(multi_outliers))
  multi_outliers$p <- pchisq(multi_outliers$mahal, df = 3, lower.tail = FALSE)

  multi_outliers2 <- filter(multi_outliers, p < 0.001)
  multi_outliers3 <- filter(multi_outliers, p < 0.001) %>% distinct()

  x <- ggplot(multi_outliers, aes(logcatch_weight, logsample_sum / 1000)) + # shows very high catch_weights
    geom_point() +
    scale_color_viridis_c(trans = "log10") #+
  #  geom_text(aes(label = as.character(fishing_event_id)))

  diag2 <- x + geom_point(multi_outliers2, mapping = aes(logcatch_weight, logsample_sum / 1000), colour = "red")

  df <- rbind(df, tibble(fishing_event_id = multi_outliers3$fishing_event_id,
                         report = "mutivariate outlier between catch weight and sample weight (mahalanobis distance"))


  # 4. find catch_weights that fall outside 3 SD
  catch_weight_SD <- y2 %>%
    distinct(fishing_event_id, .keep_all = TRUE) %>%
    below_3sd_catchweight()
  catch_weight_SD2 <- filter(catch_weight_SD, within_3sd == FALSE)
  length(unique(catch_weight_SD2$survey_desc)) # 38 surveys with catch_weights outside of the 99% CI

  df <- rbind(df, tibble(fishing_event_id = catch_weight_SD2$fishing_event_id,
                         report = "catch_weight falls outside of 3 SD of all catch weights"))

  catch_weight_3sd <- ggplot(catch_weight_SD, aes(weight_complete / 1000, catch_weight)) + # shows very high catch_weights
    geom_point() +
    # scale_x_log10() +
    # scale_y_log10() +
    scale_color_viridis_c(trans = "log10") #+
  #  geom_text(aes(label = as.character(fishing_event_id)))

  diag3 <- catch_weight_3sd + geom_point(catch_weight_SD2, mapping = aes(weight_complete / 1000, catch_weight), colour = "red")


  # 5. find sample weight values outside of 3 SD
  sample_weight_SD <- y2 %>% below_3sd()
  # x2 <- y2 %>% group_by(fishing_event_id) %>% below_2sd()
  sample_weight_SD2 <- filter(sample_weight_SD, within_3sd == FALSE)
  # length(unique(x2$survey_desc)) # 6 surveys with catch_weights outside of the 99% CI

  sample_weight_3SDplot <- ggplot(sample_weight_SD, aes(weight_complete / 1000, catch_weight)) + # shows very high catch_weights
    geom_point() +
    scale_color_viridis_c(trans = "log10") #+
  #  geom_text(aes(label = as.character(fishing_event_id)))

  diag4 <- sample_weight_3SDplot + geom_point(sample_weight_SD2, mapping = aes(weight_complete / 1000, catch_weight), colour = "red")

  df <- rbind(df, tibble(fishing_event_id = sample_weight_SD2$fishing_event_id,
                         report = "sum of sample weight per fishing id falls outside of 3 SD of all summed sample weights"))

  return(df)
}
