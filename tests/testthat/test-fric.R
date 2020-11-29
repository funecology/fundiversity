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
