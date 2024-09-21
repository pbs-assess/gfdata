# Rough - check which values differ in the unlike results

# Define -----------------------------------------------------------------------

# u <- ue

col_names <- character(0)
val_diff <- numeric(0)

# Augment ----------------------------------------------------------------------

for (i in 1:floor(nrow(u) / 2)) {

  # Construct row indexes
  i1 <- as.integer(2 * i - 1)
  i2 <- as.integer(2 * i)

  # print(paste0(i1, " ", i2))

  for (j in 2:ncol(u)) {
    # Only act if non-NA
    if (!is.na(u[i1, j]) & !is.na(u[i2, j])) {
      # Unequal values?
      if (u[i1, j] != u[i2, j]) {
        # Store colname and difference
        col_names <- c(col_names, colnames(u)[j])
        val_diff <- c(val_diff, abs(u[i1, j] - u[i2, j]))
      }
    }
  }
}
# See unique differing column names
unique(col_names)
# See value of differences
val_diff
