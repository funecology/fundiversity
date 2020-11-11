# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Functional Richness output format", {

  fric <- expect_silent(fd_fric(traits_birds))

  expect_s3_class(fric, "data.frame")
  expect_length(fric, 2)
  expect_equal(nrow(fric), 1)
  expect_equal(colnames(fric), c("site", "FRic"))

})


test_that("Functional Richness fails gracefully", {

  # No traits
  expect_error(
    fd_fric(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_raoq(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("Please provide a site-species matrix that contains all species ",
           "from your traits dataset"),
    fixed = TRUE
  )
})
