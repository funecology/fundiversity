#' Compute Functional Divergence (FDiv)
#'
#' @inheritParams fd_fric
#'
#' @inheritSection fd_fric Parallelization
#' @inherit fd_fric details
#'
#' @examples
#' data(traits_birds)
#' fd_fdiv(traits_birds)
#'
#' @return a data.frame with two columns:
#' * `site` the names of the sites as the row names of the input `sp_com`,
#' * `FDiv` the values of functional divergence at each site.
#'
#' @references
#' Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology, Ecology 89(8), \doi{10.1890/07-1206.1}
#'
#' @importFrom future.apply future_apply
#' @export
fd_fdiv <- function(traits, sp_com) {

  if (missing(traits) | is.null(traits)) {
    stop("Please provide a trait dataset", call. = FALSE)
  }

  if (is.data.frame(traits) | is.vector(traits)) {
    traits <- as.matrix(traits)
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

  # Standardize abundance per site
  site_abundances <- rowSums(sp_com)
  site_abundances[site_abundances == 0] <- 1  # Account for site with no species
  sp_com <- sp_com / site_abundances

  # Compute Functional Divergence
  fdiv_site <- future_apply(sp_com, 1, function(sp_site) {

    if (all(is.na(sp_site)) | all(sp_site == 0)) {
      return(0)
    }

    # Select only species that are in site
    sub_site <- sp_site[sp_site > 0]

    # Select traits for species actually in site
    sub_traits <- traits[names(sub_site),, drop = FALSE]

    ch <- fd_chull(sub_traits)

    verts <- ch$p[unique(c(ch$hull)),, drop = FALSE]

    G <- colMeans(verts)

    dG <- sqrt(colSums((t(sub_traits) - G)^2))

    mean_dG <- mean(dG)

    deltaD <- sum(sub_site*(dG - mean_dG))

    deltaD_abs <- sum(sub_site*abs(dG - mean_dG))

    FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)

    return(FDiv)
  })

  data.frame(site = rownames(sp_com), FDiv = fdiv_site,
             row.names = NULL)
}
