# A wrapper around geometry::intersectn() to properly handle errors and specific
# cases
fd_chull_intersect <- function(traits1, traits2) {

  traits1 <- traits1[!duplicated(traits1),, drop = FALSE]
  traits2 <- traits2[!duplicated(traits1),, drop = FALSE]

  if (ncol(traits1) == 1L & ncol(traits2) == 1L) {

    r1 <- range(traits1)
    r2 <- range(traits2)

    # Range of the overlap
    r3 <- c(max(r1[1], r2[1]),
            min(r1[2], r2[2]))

    r_overlap <- diff(r3)

    r_overlap <- ifelse(r_overlap < 0, NA_real_, r_overlap)

    return(
      list(
        ch =list(
          "hull" = r3,
          "area" = r_overlap,
          "vol" = r_overlap
        )
      )
    )
  }

  if (nrow(traits1) <= ncol(traits1) | nrow(traits2) <= ncol(traits2)) {
    return(
      list(
        ch = list(
          "hull" = seq_len(nrow(traits1) + nrow(traits2)),
          "area" = NA_real_,
          "vol" = NA_real_,
        )
      )
    )
  }

  return(geometry::intersectn(traits1, traits2, options = "FA"))

}
