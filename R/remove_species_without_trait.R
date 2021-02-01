#' Remove species with missing trait data
#'
#' @param trait_mat a matrix of trait values with species as rows and traits as
#'                  columns
#'
#' @importFrom stats complete.cases
remove_species_without_trait <- function(trait_mat) {

  complete_traits <- complete.cases(trait_mat)

  if (sum(complete_traits) != nrow(trait_mat)) {
    message("Removed ", sum(!complete_traits), " species with missing trait(s)",
            call. = FALSE)
  }

  trait_mat[complete_traits,, drop = FALSE]
}
