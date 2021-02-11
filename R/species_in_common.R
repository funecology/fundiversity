#' List of species in common between trait dataset and site-species matrix
#'
#' This functions returns the list of species in common between trait dataset
#' and site-species matrix. If no species are found in common, it errors.
#' When the species number is lower in either the trait dataset or
#' the site-species matrix it shows a message.
#'
#' @param traits  a `matrix` or `data.frame` that describe the traits of species
#'                with each species as a row and traits as columns
#' @param site_sp a `matrix`, `data.frame` or a `Matrix` with the
#'                abundances or occurrences of species in each site,
#'                with sites as rows and species as columns
#' @noRd
species_in_common = function(traits, site_sp) {
  if (!is.matrix(traits) & !is.data.frame(traits)) {
    stop("Trait dataset not of good type, check trait dataset", call. = FALSE)
  }
  if (!is.matrix(site_sp) & !is.data.frame(site_sp) &
      !inherits(site_sp, "Matrix")) {
    stop("Site-species matrix not of good type, check site-species matrix",
         call. = FALSE)
  }

  common_species <- intersect(rownames(traits), colnames(site_sp))

  if (length(common_species) == 0) {
    stop("No species in common found between trait dataset and ",
         "site-species matrix", call. = FALSE)
  }

  if (length(common_species) != nrow(traits) |
      length(common_species) != ncol(site_sp)) {
    message("Differing number of species between trait dataset ",
            "and site-species matrix\nTaking subset of species",
            appendLF = TRUE)
  }

  return(common_species)
}
