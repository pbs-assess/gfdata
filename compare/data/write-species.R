# Overview
# - Write species names to .rds file for easy use

# Write species ----------------------------------------------------------------

species_names <- c(
  "North Pacific Spiny Dogfish",
  "Pacific Ocean Perch",
  "Pacific Cod",
  "Walleye Pollock",
  "Sablefish",
  "Lingcod",
  "Bocaccio",
  "Canary Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Silvergray Rockfish",
  "Shortspine Thornyhead",
  "Widow Rockfish",
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish",
  "Copper Rockfish",
  "Shortraker Rockfish",
  "Rosethorn Rockfish",
  "Harlequin Rockfish",
  "Pygmy Rockfish",
  "Sharpchin Rockfish",
  "Darkblotched Rockfish",
  "Greenstriped Rockfish",
  "Petrale Sole",
  "Arrowtooth Flounder",
  "English Sole",
  "Dover Sole",
  "Rex Sole",
  "Flathead Sole",
  "Southern Rock Sole",
  "Slender Sole",
  "Pacific Sanddab",
  "Pacific Halibut",
  "Butter Sole",
  "Pacific Hake",
  "Pacific Tomcod",
  "Spotted Ratfish",
  "Longnose Skate",
  "Big Skate",
  "Sandpaper Skate",
  "Curlfin Sole",
  "Sand Sole"
)

# Write
saveRDS(species_names, file = "compare/data/species.rds")

# End --------------------------------------------------------------------------
