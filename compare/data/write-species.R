# Overview
# - Write species names to .rds file for easy use

# Write species ----------------------------------------------------------------

species_names <- c(
  # "Spotted Ratfish",
  # "Longnose Skate",
  "Big Skate",
  "Eulachon",
  # "Sandpaper Skate",
  "North Pacific Spiny Dogfish",
  "Pacific Ocean Perch",
  "Pacific Cod",
  # "Walleye Pollock",
  # "Sablefish",
  "Lingcod",
  # "Pacific Hake",
  # "Pacific Tomcod",
  # "Bocaccio",
  # "Canary Rockfish",
  # "Copper Rockfish",
  # "Darkblotched Rockfish",
  # "Greenstriped Rockfish",
  # "Harlequin Rockfish",
  # "Pygmy Rockfish",
  "Quillback Rockfish",
  # "Redbanded Rockfish",
  # "Redstripe Rockfish",
  # "Rosethorn Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  # "Sharpchin Rockfish",
  # "Silvergray Rockfish",
  # "Widow Rockfish",
  "Yelloweye Rockfish",
  # "Yellowmouth Rockfish",
  # "Yellowtail Rockfish",
  # "Shortraker Rockfish",
  "Shortspine Thornyhead",
  # "Arrowtooth Flounder",
  # "Butter Sole",
  # "Curlfin Sole",
  # "Dover Sole",
  # "English Sole",
  # "Flathead Sole",
  "Pacific Halibut",
  # "Pacific Sanddab",
  "Petrale Sole",
  # "Rex Sole",,
  # "Sand Sole",
  "Slender Sole" # ,
  # "Southern Rock Sole"
)

# Write
saveRDS(species_names, file = "compare/data/species.rds")

# End --------------------------------------------------------------------------
