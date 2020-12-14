#' Compute Functional Divergence (FDiv)
#'
#' @inheritParams fd_fric
#'
#' @examples
#' data(traits_birds)
#' fd_fdiv(traits_birds)
#'
#' @return The value of FDiv (numeric of length 1)
#'
#' @references
#' Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology, Ecology 89(8), \doi{10.1890/07-1206.1}
#'
#' @export
fd_fdiv <- function(traits, sp_com) {

  if (missing(traits) | is.null(traits)) {
    stop("Please provide a trait dataset", call. = FALSE)
  }

  if (is.data.frame(traits) | is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (!missing(sp_com)) {

    common_species <- species_in_common(traits, sp_com)

    traits <- traits[common_species,, drop = FALSE]
    sp_com <- sp_com[, common_species, drop = FALSE]

  } else {

    sp_com <- matrix(1, ncol = nrow(traits),
                     dimnames = list("s1", row.names(traits)))

  }

  # Standardize abundance per site
  sp_com <- sp_com / rowSums(sp_com)

  # Compute Functional Divergence
  fdiv_site <- apply(sp_com, 1, function(sp_site) {

    # Select only species that are in site
    sub_site <- sp_site[sp_site > 0]

    # Select traits for species actually in site
    sub_traits <- traits[names(sub_site),, drop = FALSE]

    G <- colMeans(sub_traits, na.rm = TRUE)

    dG <- sqrt(colSums((t(sub_traits) - G)^2, na.rm = TRUE))

    mean_dG <- mean(dG)

    deltaD <- sum(sub_site*(dG - mean_dG))

    deltaD_abs <- sum(sub_site*abs(dG - mean_dG))

    FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)
  })

  data.frame(site = row.names(sp_com), FDiv = fdiv_site)

}
