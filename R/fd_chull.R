# A wrapper around geometry::convhulln() to properly handle errors and specific
# cases
fd_chull <- function(traits) {

  traits <- traits[!duplicated(traits),, drop = FALSE]

  if (nrow(traits) <= ncol(traits)) {
    return(NA_real_)
  }

  return(geometry::convhulln(traits, "FA"))

}
