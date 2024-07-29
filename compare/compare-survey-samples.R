# Overview
# - Script iterates over species and ssids
# - Compares specimen_id column for extras in each data frame
# - Identifies rows indexes for extra specimen_id values
# - Accumulates extra specimen_id rows for each iteration
#
# Wish list
# - Update get_survey_samples*() functions to avoid errors when no data
# - E.g. spp <- "009"; ssid <- 1; returns errors for both functions

# Load packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all(".")
source(here::here("compare", "R", "utils.R"))

# Get ssids --------------------------------------------------------------------
id_tab <- readRDS("compare/data/ssids.rds")
ssid <- id_tab |> dplyr::pull(ssid)

# Get species ------------------------------------------------------------------

spp <- readRDS("compare/data/species-names.rds")

# Shorter species set
spp <- "225" # Hake
spp <- "044" # Dogfish
spp <- "009" # Rougheye

# Compare specimens ------------------------------------------------------------

cs <- compare_specimens(spp = spp, ssid = ssid)

# Errored calls
e1 <- cs$e1 |> left_join(id_tab, by = "ssid")
e1 |> view()
e1 |> dplyr::filter(returned == "no")

e2 <- cs$e2 |> left_join(id_tab, by = "ssid")
e2 |> view()
e2 |> dplyr::filter(returned == "no")

# Extra specimens
s1 <- cs$s1
s1 |> nrow()

s2 <- cs$s2
s2 |> nrow()



# TODO Continue from here


# Compare columns --------------------------------------------------------------

# TODO: Compare columns
# TODO: Store column values that don't match





# Iterate over cases
for (i in seq_along(spp)) {
  for (j in seq_along(ids)) {
    # Reset value
    d1 <- NULL
    d2 <- NULL
    # Pull data
    try(d1 <- gfdata::get_survey_samples(species = spp[i], ssid = ids[j]))
    try(d2 <- gfdata::get_survey_samples2(species = spp[i], ssid = ids[j]))
    # Check value
    if (is.null(d1)) {
      # Document error
      e1 <- rbind(e1, tibble(spp = spp[i], ssid = ids[j], ssids[j, 2:3]))
    }
    if (is.null(d2)) {
      # Document error
      e2 <- rbind(e2, tibble(spp = spp[i], ssid = ids[j], ssids[j, 2:3]))
    }
    # Identify extra specimen_id
    n1 <- setdiff(d1$specimen_id, d2$specimen_id)
    n2 <- setdiff(d2$specimen_id, d1$specimen_id)
    # Identify and store extra specimen_id rows
    if (length(n1) > 0) {
      r1 <- which(d1$specimen_id %in% n1)
      s1 <- rbind(s1, tibble::tibble(spp = spp[i], ssid = ids[j], d1[r1, ]))
    }
    if (length(n2 > 0)) {
      r2 <- which(d2$specimen_id %in% n2)
      s2 <- rbind(s2, tibble::tibble(spp = spp[i], ssid = ids[j], d2[r2, ]))
    }
  }
}



# Get colnames -----------------------------------------------------------------

# saveRDS(colnames(s1), "compare/data/colnames-samples.rds")
# saveRDS(colnames(s2), "compare/data/colnames-samples2.rds")

# Get colnames
# c1 <- readRDS("compare/data/colnames-samples.rds")
# c2 <- readRDS("compare/data/colnames-samples2.rds")







