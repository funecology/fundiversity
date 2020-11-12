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

    if (!all(row.names(traits) %in% colnames(sp_com))) {
      stop(
        "Please provide a site-species matrix that contains all species ",
        "from your trait dataset", call. = FALSE
      )
    }

    sp_com <- sp_com[, row.names(traits), drop = FALSE]

  } else {

    sp_com <- matrix(1, ncol = nrow(traits),
                     dimnames = list("s1", row.names(traits)))

  }

  # Standardize abundance per site
  sp_com <- sweep(sp_com, 1, rowSums(sp_com), "/")

  # Compute Functional Divergence
  fdiv_site <- apply(sp_com, 1, function(site_row) {

    # Sub-select present species
    sub_abund <- site_row[site_row > 0]
    # Select traits for species actually in site
    sub_traits <- traits[site_row > 0,, drop = FALSE]

    G <- colMeans(sub_traits, na.rm = TRUE)

    dG <- sqrt(colSums((t(sub_traits) - G)^2, na.rm = TRUE))

    mean_dG <- mean(dG)

    deltaD <- sum(sub_abund*(dG - mean_dG))

    deltaD_abs <- sum(sub_abund*abs(dG - mean_dG))

    FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)
  })

  data.frame(site = row.names(sp_com), FDiv = fdiv_site)

}
