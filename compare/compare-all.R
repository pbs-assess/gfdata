# Compare specimens and then sets
{
  source(here::here("compare", "compare-survey-samples.R"))
  source(here::here("compare", "compare-survey-sets.R"))
}


library(tidyverse)

# Read samples files
xa <- readRDS("compare/results/samples-extras.rds")
va <- readRDS("compare/results/samples-unlike.rds")
sa <- readRDS("compare/results/samples-summary.rds")

# Read sets files
xe <- readRDS("compare/results/sets-extras.rds")
ve <- readRDS("compare/results/sets-unlike.rds")
se <- readRDS("compare/results/sets-summary.rds")
