# Compare specimens and then sets
{
  source(here::here("compare", "compare-survey-samples.R"))
  source(here::here("compare", "compare-survey-sets.R"))
}


library(tidyverse)

# Read specimen files
e1 <- readRDS("compare/results/errors-samples.rds")
e2 <- readRDS("compare/results/errors-samples2.rds")
s1 <- readRDS("compare/results/extras-samples.rds")
s2 <- readRDS("compare/results/extras-samples2.rds")
u12 <- readRDS("compare/results/unlike-samples.rds")

# View
view(e1)
view(e2)
view(s1)
view(s2)
view(u12)

# Read specimen files
r1 <- readRDS("compare/results/errors-sets.rds")
r2 <- readRDS("compare/results/errors-sets2.rds")
x1 <- readRDS("compare/results/extras-sets.rds")
x2 <- readRDS("compare/results/extras-sets2.rds")
v12 <- readRDS("compare/results/unlike-sets.rds")

# View
view(r1)
view(r2)
view(x1)
view(x2)
view(v12)
