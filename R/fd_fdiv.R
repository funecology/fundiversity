#' Compute Functional Divergence (FDiv)
#'
#' This function computes Functional Divergence (FDiv) following Villéger et al.
#' (2008). NB: when a site contains no species FDiv is equal to 0. If for a site
#' there are less traits than species, then FDiv is equal to `NaN`.
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
#' ```{r child = "man/rmdchunks/no_row_names.Rmd"}
#' ```
#'
#' NB: when a site contains no species FDiv is equal to 0. If for a site
#' there are less traits than species, then FDiv is equal to `NaN`.
#'
#' @references
#' Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology, Ecology 89(8), \doi{10.1890/07-1206.1}
#'
#' @importFrom future.apply future_apply
#' @export
fd_fdiv <- function(traits, sp_com) {

  if (missing(traits) || is.null(traits)) {
    stop("Please provide a trait dataset", call. = FALSE)
  }

  if (is.data.frame(traits) || is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (!is.numeric(traits)) {
    stop("Non-continuous trait data found in input traits. ",
         "Please provide only continuous trait data", call. = FALSE)
  }

  traits <- remove_species_without_trait(traits)

  if (!missing(sp_com)) {

    common_species <- species_in_common(traits, sp_com)

    traits <- traits[common_species,, drop = FALSE]
    sp_com <- sp_com[, common_species, drop = FALSE]

  } else {

    if (is.null(rownames(traits))) {
      rownames(traits) <- paste0("sp", seq_len(nrow(traits)))
   }
    sp_com <- matrix(1, ncol = nrow(traits),
                     dimnames = list("s1", rownames(traits)))

  }

  if (is.null(rownames(sp_com))) {

    rownames(sp_com) <- paste0("s", seq_len(nrow(sp_com)))

  }

  # Standardize abundance per site
  site_abundances <- rowSums(sp_com, na.rm = TRUE)
  site_abundances[site_abundances == 0] <- 1  # Account for site with no species
  sp_com <- sp_com / site_abundances

  convex_hull <- if (use_memoise()) {
    fd_chull_memoised
  } else {
    fd_chull
  }

  # Compute Functional Divergence
  fdiv_site <- future_apply(sp_com, 1, function(sp_site) {

    if (all(is.na(sp_site)) | all(sp_site == 0)) {
      return(0)
    }

    # Select only species that are in site
    sub_site <- sp_site[sp_site > 0]

    # Select traits for species actually in site
    sub_traits <- traits[names(sub_site),, drop = FALSE]

    ch <- convex_hull(sub_traits)

    verts <- ch$p[unique(c(ch$hull)),, drop = FALSE]

    G <- colMeans(verts)

    dG <- sqrt(colSums((t(sub_traits) - G)^2))

    mean_dG <- mean(dG)

    deltaD <- sum(sub_site*(dG - mean_dG))

    deltaD_abs <- sum(sub_site*abs(dG - mean_dG))

    FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)

    return(FDiv)
  }, future.globals = FALSE)

  data.frame(site = rownames(sp_com), FDiv = fdiv_site,
             row.names = NULL)
}
