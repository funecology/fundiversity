# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Functional Richness output format", {

  fric <- expect_silent(fd_fric(traits_birds))

  expect_type(fric, "numeric")
  expect_length(fric, 1)

})

test_that("Functional Richness works in 1D", {

  expect_silent(fd_fric(traits_birds[, 1]))

})

test_that("Functional Richness computes correct value", {

  expect_equal(fd_fric(traits_birds_sc), 88.9286, tolerance = 1e-4)

})
