# Preamble code
data("traits_birds")

# Actual tests
test_that("Functional Richness output format", {

  fric <- expect_silent(fd_fric(traits_birds))

  expect_s3_class(fric, "data.frame")
  expect_length(fric, 2)
  expect_equal(nrow(fric), 1)
  expect_equal(colnames(fric), c("site", "FRic"))

  expect_equal(fd_fric(traits_birds)$FRic, 230967.7, tolerance = 1e-6)
})

test_that("Functional Richness works in 1D", {

  expect_identical(
    fd_fric(traits_birds[, 1]),
    fd_fric(traits_birds[, 1, drop = FALSE])
  )

  expect_identical(
    fd_fric(traits_birds[, 1], stand = TRUE),
    fd_fric(traits_birds[, 1, drop = FALSE], stand = TRUE)
  )

})

test_that("Function Richness works on subset of site/species", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  expect_message(fd_fric(traits_birds, site_sp[, 2:ncol(site_sp),
                                               drop = FALSE]),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))

  expect_message(fd_fric(traits_birds[2:nrow(traits_birds),], site_sp),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))
})


test_that("Functional Richness can standardize its values", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp2 <- rbind(site_sp, site_sp)
  rownames(site_sp2) <- paste0("s", 1:2)
  site_sp2[2, 120] <- 0  # Remove Penelope jacquacy which is the biggest bird

  fric <- fd_fric(traits_birds, stand = TRUE)
  fric_low_1 <- suppressMessages(fd_fric(traits_birds, site_sp2, stand = TRUE))

  expect_equal(fric$FRic[[1]], 1)
  expect_equal(fric_low_1$FRic[[1]], 1)
  expect_lt(fric_low_1$FRic[[2]], 1)
})

test_that("Functional Richness edge cases", {

  # Not enough species compared to the number of traits to be computed
  expect_identical(
    fd_fric(traits_birds[1:4, ])[["FRic"]],
    NA_real_
  )

  # Several species with similar trait values -> not enought species for FRic
  dup_traits <- lapply(1:5, function(x) traits_birds[1,, drop = FALSE])
  dup_traits <- do.call(rbind, dup_traits)

  expect_identical(fd_fric(dup_traits)[["FRic"]], NA_real_)

  # Trying to compute FRic with >16 traits
  many_traits <- cbind(traits_birds, traits_birds, traits_birds, traits_birds,
                       traits_birds)
  expect_error(
    fd_fric(many_traits),
    paste0(
      "Due to computational limits FRic can only be computed with n <= 16 ",
      "traits\nConsider dimension reduction techniques if you have more than",
      " 16 traits"),
    fixed = TRUE
  )

  # Compute FRic for site with a single species
  site_sp <- matrix(0, ncol = nrow(traits_birds), nrow = 1,
                    dimnames = list("s1", rownames(traits_birds)))
  site_sp[1,1] <- 1

  expect_silent(fd_fric(traits_birds, site_sp))

  expect_identical(fd_fric(traits_birds, site_sp)[["FRic"]], NA_real_)

})

test_that("Functional Richness works on sparse matrices", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  sparse_site_sp <- as(site_sp, "sparseMatrix")

  expect_silent(fd_fric(traits_birds, sparse_site_sp))

  expect_equal(fd_fric(traits_birds, sparse_site_sp)$FRic, 230967.7,
               tolerance = 1e-6)
})

test_that("Functional Richness fails gracefully", {

  # No traits
  expect_error(
    fd_fric(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_fric(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("No species in common found between trait dataset ",
           "and site-species matrix"),
    fixed = TRUE
  )
})
