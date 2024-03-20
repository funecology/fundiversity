# Preamble code ----------------------------------------------------------------
data("traits_birds")
data("site_sp_birds")


# Tests for valid inputs -------------------------------------------------------

test_that("Functional Richness Intersection output format", {

  fric_int <- expect_silent(fd_fric_intersect(traits_birds))

  expect_s3_class(fric_int, "data.frame")
  expect_identical(dim(fric_int), c(1L, 3L))
  expect_named(fric_int, c("first_site", "second_site", "FRic_intersect"))

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
    fd_fric_intersect(traits_birds[, 1, drop = FALSE], site_sp_birds,
                      stand = TRUE)
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

  expect_identical(fric_int$FRic_intersect[[1]], 1)
  expect_identical(fric_int_low_1$FRic_intersect[[2]], 1)
  expect_lt(fric_int_low_1$FRic_intersect[[3]], 1)
  expect_lt(fric_int_low_1$FRic_intersect[[1]], 1)
})

test_that("Functional Richness Intersection edge cases", {

  # n_species < n_traits
  expect_warning(
    fric_inter <- fd_fric_intersect(traits_birds[1:4, ]),
    "Some sites had less species than traits so returned FRic_intersect is 'NA'"
  )

  expect_identical(fric_inter[["FRic_intersect"]], NA_real_)

  expect_message(
    expect_setequal(
      suppressWarnings({
        fd_fric_intersect(
          traits_birds[1:4, ], site_sp_birds)[["FRic_intersect"]]
      }), NA_real_
    ),
    paste0(
      "Differing number of species between trait dataset and site-species ",
      "matrix\nTaking subset of species"
    ),
    fixed = TRUE
  )


  # Duplicate trait values -> n_species < n_traits
  dup_traits <- lapply(1:5, function(x) traits_birds[1,, drop = FALSE])
  dup_traits <- do.call(rbind, dup_traits)

  expect_warning(
    inter_dup <- fd_fric_intersect(dup_traits),
    "Some sites had less species than traits so returned FRic_intersect is 'NA'"
  )

  expect_identical(inter_dup[["FRic_intersect"]], NA_real_)


  # n_species = 1 < n_traits
  site_sp <- matrix(0, ncol = nrow(traits_birds), nrow = 1,
                    dimnames = list("s1", rownames(traits_birds)))
  site_sp[1,1] <- 1

  expect_warning(
    fric_inter <- fd_fric_intersect(traits_birds, site_sp),
    "Some sites had less species than traits so returned FRic_intersect is 'NA'"
  )

  expect_identical(fric_inter[["FRic_intersect"]], NA_real_)


  # n_species = 0 < n_traits
  data("traits_plants")
  data("site_sp_plants")

  expect_warning(
    fric_inter <- fd_fric_intersect(
      traits_plants, site_sp_plants[10,, drop = FALSE]
    ),
    "Some sites had less species than traits so returned FRic_intersect is 'NA'"
  )

  expect_identical(fric_inter[["FRic_intersect"]], NA_real_)

  # n_traits > 16 (computational limit)
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

test_that("Functional Richness Inters. works on data.frame as well as matrix", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp_df <- as.data.frame(site_sp)

  fric_intersect    <- expect_silent(
    fd_fric_intersect(traits_birds, site_sp)
  )
  fric_intersect_df <- expect_silent(
    fd_fric_intersect(traits_birds, site_sp_df)
  )

  expect_s3_class(fric_intersect, "data.frame")
  expect_identical(dim(fric_intersect), c(1L, 3L))
  expect_named(fric_intersect, c("first_site", "second_site", "FRic_intersect"))

  expect_equal(
    fric_intersect,
    fric_intersect_df
  )
})

test_that("Functional Richness Intersect works with unmemoised version", {

  withr::local_options(fundiversity.memoise = FALSE)

  fric_int <- expect_silent(fd_fric_intersect(traits_birds))

  expect_s3_class(fric_int, "data.frame")
  expect_identical(dim(fric_int), c(1L, 3L))
  expect_named(fric_int, c("first_site", "second_site", "FRic_intersect"))

  expect_equal(fd_fric_intersect(traits_birds, stand = TRUE)$FRic_intersect,
               1, tolerance = 1e-6)
})

# Tests for invalid inputs -----------------------------------------------------

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

  ## Categorical trait data
  # Add non-continuous traits
  traits_birds_cat <- as.data.frame(traits_birds)
  traits_birds_cat$cat_trait <- "a"

  expect_error(
    fd_fric_intersect(traits_birds_cat, site_sp_birds),
    paste0("Non-continuous trait data found in input traits. ",
           "Please provide only continuous trait data"),
    fixed = TRUE
  )
})
