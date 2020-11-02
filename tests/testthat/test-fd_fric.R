test_that("fd_fric", {

  data(traits_birds)

  fric <- expect_silent(fd_fric(traits_birds))

  expect_type(fric, numeric)
  expect_length(fric, 1)

})
