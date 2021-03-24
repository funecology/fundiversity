#' Intersection between convex hulls of pairs of sites
#'
#' Compute volume of the intersection of the convex hulls of all pairs
#' of sites (including self-intersection, which corresponds to their convex
#' hull). Note that when standardizing convex hulls of intersections,
#' this function uses the convex hull of all provided traits,
#' thus standardized volume of self-intersection hulls can be lower than one.
#'
#' @inheritParams fd_fric
#'
#' @return a data.frame with three columns:
#' * `first_site` the names of the first site used in the pair `sp_com`,
#' * `second_site` the names of the first site used in the pair,
#' * `FRic_intersect` the volume of the convex hulls intersection of each
#' pair of site.
#'
#' @seealso [fd_fric()], [geometry::intersectn()], [geometry::convhulln()]
#'
#' @importFrom utils combn
#' @importFrom future.apply future_apply
#'
#' @examples
#' data(traits_birds)
#' fd_fric_intersect(traits_birds)
#'
#' @export
fd_fric_intersect = function(traits, sp_com, stand = FALSE) {

  if (missing(traits) | is.null(traits)) {
    stop("Please provide a trait dataset", call. = FALSE)
  }

  if (is.data.frame(traits) | is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (ncol(traits) > 16) {
    stop("Due to computational limits FRic intersect can only be computed ",
         "with n <= 16 traits\nConsider dimension reduction techniques ",
         "if you have more than 16 traits")
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

  # All pairs of sites (not within themselves)
  if (nrow(sp_com) >= 2) {
    all_site_comb <- t(combn(rownames(sp_com), 2))
  } else {
    all_site_comb <- NULL
  }

  self_intersection <- matrix(rep(rownames(sp_com), each = 2),
                              byrow = TRUE, ncol = 2)

  all_site_comb <- rbind(all_site_comb, self_intersection)


  fric_intersect <- future_apply(all_site_comb, 1, function(site_comb) {

    first_row     <- sp_com[site_comb[[1]],, drop = TRUE]
    first_traits  <- traits[first_row > 0,,  drop = FALSE]

    # Compute intersections
    if (site_comb[[1]] != site_comb[[2]]) {
      # True intersections

      second_row    <- sp_com[site_comb[[2]],, drop = TRUE]
      second_traits <- traits[second_row > 0,, drop = FALSE]

      fd_chull_intersect(first_traits, second_traits)$vol
    } else {
      # Self-intersection (equivalent to regular convex hulls)
      # way more efficient that compute with fd_chull_inters
      fd_chull(first_traits)$vol
    }
  })

  data.frame(first_site = all_site_comb[,1],
             second_site = all_site_comb[,2],
             FRic_intersect = fric_intersect/max_range,
             row.names = NULL)
}