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
