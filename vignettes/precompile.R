# Pre-compiled vignettes that take a long time to build and require network access
# Only run this if you're on network, otherwise you will overwrite the data stored in the the output file.
knitr::knit("vignettes/gfdata-vignette.Rmd.orig", output = "vignettes/01-gfdata-vignette.Rmd")
knitr::knit("vignettes/gfdata-vignette-get-all.Rmd.orig", output = "vignettes/02-gfdata-vignette-get-all.Rmd")

