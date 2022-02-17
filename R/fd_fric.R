#' Compute Functional Richness (FRic)
#'
#' Functional Richness is computed as the volume of the convex hull from all
#' included traits.
#'
#' @param traits The matrix dataset for which you want to compute the index
#' @param sp_com Site-species matrix with sites as rows and species as columns
#'               if not provided, the function considers all species with equal
#'               abundance in a single site. This can be either a `matrix`,
#'               a `data.frame`, or a [Matrix::Matrix()] object.
#' @param stand  a boolean indicating whether to standardize FRic values over
#'               the observed maximum over all species (default: `FALSE`).
#'               This scales FRic between 0 and 1.
#'               **NB**: The maximum FRic values only considers species that are
#'               present in **both** site-species and trait matrices.
#'               If you want to consider species that are absent
#'               in the site-species matrix, add corresponding columns of 0s.
#'
#' @section Parallelization:
#' The computation of this function can be parallelized thanks to
#' [future::plan()]. To get more information on how to parallelize your
#' computation please refer to the parallelization vignette with:
#' `vignette("parallel", package = "fundiversity")`
#'
#' @examples
#' data(traits_birds)
#' fd_fric(traits_birds)
#'
#' data(site_sp_birds)
#' fd_fric(traits_birds, site_sp_birds)
#'
#' @details By default, when loading \pkg{fundiversity}, the functions to
#' compute convex hulls are
#' [memoised](https://en.wikipedia.org/wiki/Memoization) through the `memoise`
#' package if it is installed. To deactivate this behavior you can set the
#' option `fundiversity.memoise` to `FALSE` by running the following line:
#' `options(fundiversity.memoise = FALSE)`. If you use it interactively it will
#' only affect your current session. Add it to your script(s) or `.Rprofile`
#' file to avoid toggling it each time.
#'
#' @return a data.frame with two columns:
#' * `site` the names of the sites as the row names of the input `sp_com`,
#' * `FRic` the values of functional richness at each site.
#'
#' NB: FRic is equal to `NA` when there are strictly less species in a site
#' than the number of provided traits.
#'
#' @references
#' Cornwell W. K., Schwilk D. W., Ackerly D. D. (2006), A trait-based test for
#' habitat filtering; convex hull volume, Ecology 84(6),
#' \doi{10.1890/0012-9658(2006)87[1465:ATTFHF]2.0.CO;2}
#'
#' @importFrom future.apply future_apply
#' @export
fd_fric <- function(traits, sp_com, stand = FALSE) {

  if (missing(traits) | is.null(traits)) {
    stop("Please provide a trait dataset", call. = FALSE)
  }

  if (is.data.frame(traits) | is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (ncol(traits) > 16) {
    stop("Due to computational limits FRic can only be computed with n <= 16 ",
         "traits\nConsider dimension reduction techniques if you have more ",
         "than 16 traits")
  }

  traits <- remove_species_without_trait(traits)

  if (!missing(sp_com)) {

    common_species <- species_in_common(traits, sp_com)

    traits <- traits[common_species,, drop = FALSE]
    sp_com <- sp_com[, common_species, drop = FALSE]


  } else {

    sp_com <- matrix(1, ncol = nrow(traits),
                     dimnames = list("s1", rownames(traits)))

  }

  max_range <- 1

  if (stand) {
    max_range <- fd_chull(traits)$vol
  }

  fric_site <- future_apply(sp_com, 1, function(site_row) {
    fd_chull(traits[site_row > 0,, drop = FALSE])$vol
  })

  data.frame(site = rownames(sp_com), FRic = fric_site/max_range,
             row.names = NULL)
}
