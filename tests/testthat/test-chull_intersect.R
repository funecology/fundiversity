test_that("memoise options work on fd_chull_intersect()", {
  skip_if_not_installed("memoise")

  expect_true(memoise::is.memoised(fd_chull_intersect))

  data("traits_birds")
  ch_i <- fd_chull_intersect(traits_birds, traits_birds)

  expect_true(
    memoise::has_cache(fd_chull_intersect)(traits_birds, traits_birds)
  )

  options(fundiversity.memoise = FALSE)
  expect_false(getOption("fundiversity.memoise", TRUE))

  fundiversity:::.onLoad(system.file(package = "fundiversity"), "fundiversity")
  expect_false(memoise::is.memoised(fd_chull_intersect))

  # Reset the option to TRUE
  options(fundiversity.memoise = TRUE)
  expect_true(getOption("fundiversity.memoise", TRUE))

  fundiversity:::.onLoad(system.file(package = "fundiversity"), "fundiversity")
  expect_true(memoise::is.memoised(fd_chull_intersect))

  ch_i2 <- fd_chull_intersect_memoised(traits_birds[2:5,], traits_birds[2:5,])

  expect_true(
    memoise::has_cache(fd_chull_intersect)(traits_birds[2:5,],
                                                    traits_birds[2:5,])
  )
})
