# A wrapper around geometry::convhulln() to properly handle errors and specific
# cases
fd_chull <- function(traits) {

  traits <- traits[!duplicated(traits),, drop = FALSE]

  if (ncol(traits) == 1L) {
   return(list(
     "hull" = c(which.min(traits), which.max(traits)),
     "area" = max(traits) - min(traits),
     "vol" = max(traits) - min(traits),
     p = traits
   ))
  }

  if (nrow(traits) <= ncol(traits)) {
    return(list(
      "hull" = seq_len(nrow(traits)),
      "area" = NA_real_,
      "vol" = NA_real_,
      p = traits
    ))
  }

  return(geometry::convhulln(traits, "FA"))

}

fd_chull_memoised <- memoise::memoise(fd_chull)
