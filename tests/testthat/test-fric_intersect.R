# Preamble code
data("traits_birds")
data("site_sp_birds")

# Actual tests
test_that("Functional Richness Intersection output format", {

  fric_int <- expect_silent(fd_fric_intersect(traits_birds))

  expect_s3_class(fric_int, "data.frame")
  expect_length(fric_int, 3)
  expect_equal(nrow(fric_int), 1)
  expect_equal(colnames(fric_int), c("first_site", "second_site",
                                     "FRic_intersect"))

  expect_equal(fd_fric_intersect(traits_birds, stand = TRUE)$FRic_intersect,
               1, tolerance = 1e-6)
})

test_that("Functional Richness Intersection works in 1D", {

  expect_identical(
    fd_fric_intersect(traits_birds[, 1], site_sp_birds),
    fd_fric_intersect(traits_birds[, 1, drop = FALSE], site_sp_birds)
  )

  expect_identical(
    fd_fric_intersect(traits_birds[, 1], site_sp_birds, stand = TRUE),
    fd_fric_intersect(traits_birds[, 1, drop = FALSE], site_sp_birds, stand = TRUE)
  )

})

test_that("Function Richness Intersection works on subset of site/species", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  expect_message(
    fd_fric_intersect(traits_birds, site_sp[, 2:ncol(site_sp), drop = FALSE]),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species"))

  expect_message(
    fd_fric_intersect(traits_birds[2:nrow(traits_birds),], site_sp),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species")
  )
})

test_that("Functional Richness Intersection works for sites with no species", {
  data("traits_plants")
  data("site_sp_plants")

  fric_inter <- expect_silent(
    fd_fric_intersect(traits_plants, site_sp_plants[10,, drop = FALSE])
  )

  expect_equal(fric_inter$FRic_intersect[[1]], NA_real_)
})

test_that("Functional Richness Intersection can standardize its values", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp2 <- rbind(site_sp, site_sp)
  rownames(site_sp2) <- paste0("s", 1:2)
  site_sp2[2, 120] <- 0  # Remove Penelope jacquacy which is the biggest bird

  fric_int <- fd_fric_intersect(traits_birds, stand = TRUE)
  fric_int_low_1 <- suppressMessages(fd_fric_intersect(traits_birds, site_sp2,
                                                   stand = TRUE))

  expect_equal(fric_int$FRic_intersect[[1]], 1)
  expect_equal(fric_int_low_1$FRic_intersect[[2]], 1)
  expect_lt(fric_int_low_1$FRic_intersect[[3]], 1)
  expect_lt(fric_int_low_1$FRic_intersect[[1]], 1)
})

test_that("Functional Richness Intersection edge cases", {

  # Not enough species compared to the number of traits to be computed
  expect_identical(
    fd_fric_intersect(traits_birds[1:4, ])[["FRic_intersect"]],
    NA_real_
  )

  expect_message(
    expect_setequal(
      fd_fric_intersect(traits_birds[1:4, ], site_sp_birds)[["FRic_intersect"]],
      NA_real_
    )
  )

  # Several species with similar trait values -> not enought species for FRic
  dup_traits <- lapply(1:5, function(x) traits_birds[1,, drop = FALSE])
  dup_traits <- do.call(rbind, dup_traits)

  expect_identical(fd_fric_intersect(dup_traits)[["FRic_intersect"]], NA_real_)

  # Trying to compute FRic with >16 traits
  many_traits <- cbind(traits_birds, traits_birds, traits_birds, traits_birds,
                       traits_birds)
  expect_error(
    fd_fric_intersect(many_traits),
    paste0(
      "Due to computational limits FRic intersect can only be computed ",
      "with n <= 16 traits\nConsider dimension reduction techniques ",
      "if you have more than 16 traits"),
    fixed = TRUE
  )

  # Compute FRic for site with a single species
  site_sp <- matrix(0, ncol = nrow(traits_birds), nrow = 1,
                    dimnames = list("s1", rownames(traits_birds)))
  site_sp[1,1] <- 1

  expect_silent(fd_fric_intersect(traits_birds, site_sp))

  expect_identical(fd_fric_intersect(traits_birds, site_sp)[["FRic_intersect"]],
                   NA_real_)

})

test_that("Functional Richness Intersection works on sparse matrices", {

  skip_if_not_installed("Matrix")

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  sparse_site_sp <- Matrix(site_sp, sparse = TRUE)

  expect_silent(fd_fric_intersect(traits_birds, sparse_site_sp))

  expect_equal(
    fd_fric_intersect(traits_birds, sparse_site_sp, stand = TRUE)$FRic_intersect,
    1, tolerance = 1e-6)
})

test_that("Functional Richness Intersection fails gracefully", {

  # No traits
  expect_error(
    fd_fric_intersect(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_fric_intersect(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("No species in common found between trait dataset ",
           "and site-species matrix"),
    fixed = TRUE
  )
})
