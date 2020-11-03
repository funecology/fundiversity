# Preamble code
data("traits_birds")
traits_birds_sc = scale(traits_birds)

# Actual tests
test_that("Functional Richness computes correct value", {
  expect_equal(fd_fric(traits_birds_sc), 88.9286, tolerance = 1e-4)
})
