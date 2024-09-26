# Overview
# - Write species names to .rds file for easy use

# Write species ----------------------------------------------------------------

species_names <- c(
  "North Pacific Spiny Dogfish",
  "Eulachon"
)

# Write
saveRDS(species_names, file = "compare/data/species-sets.rds")

# End --------------------------------------------------------------------------
