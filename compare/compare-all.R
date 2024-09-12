# Compare specimens and then sets
{
  source(here::here("compare", "compare-survey-samples.R"))
  source(here::here("compare", "compare-survey-sets.R"))
}


library(tidyverse)

# Read samples files
xa <- readRDS("compare/results/samples-extras.rds")
ua <- readRDS("compare/results/samples-unlike.rds")
sa <- readRDS("compare/results/samples-summary.rds")
aa <- readRDS("compare/results/samples-alldiff.rds")

# Read sets files
xe <- readRDS("compare/results/sets-extras.rds")
ue <- readRDS("compare/results/sets-unlike.rds")
se <- readRDS("compare/results/sets-summary.rds")
ae <- readRDS("compare/results/sets-alldiff.rds")
