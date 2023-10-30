delayedAssign("mssm_grid_sf", local({
  requireNamespace("sf", quietly = TRUE)
  gfdata:::mssm_grid_sf
}))
