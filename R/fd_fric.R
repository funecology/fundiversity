#' Compute Functional Richness (FRic)
#'
#' Functional Richness is computed as the volume of the convex hull from all
#' included traits.
#'
#' @param traits The matrix dataset for which you want to compute the index
#' @param sp_com Site-species matrix with sites as rows and species as columns
#'               if not provided, the function considers all species with equal
#'               abundance in a single site
#'
#' @examples
#' data(traits_birds)
#' fd_fric(traits_birds)
#'
#' @return The value of FRic (numeric of length 1)
#'
#' @references
#' Cornwell W. K., Schwilk D. W., Ackerly D. D. (2006), A trait-based test for
#' habitat filtering; convex hull volume, Ecology 84(6),
#' \doi{10.1890/0012-9658(2006)87[1465:ATTFHF]2.0.CO;2}
#'
#' @export
fd_fric <- function(traits, sp_com) {

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

  if (ncol(traits) == 1L) {

    fric_site <- apply(sp_com, 1, function(site_row) {
      diff(range(traits[site_row > 0,]))
    })

  } else {

    fric_site <- apply(sp_com, 1, function(site_row) {
      geometry::convhulln(traits[site_row > 0,], "FA")$vol
    })
  }

  data.frame(site = row.names(sp_com), FRic = fric_site)
}
