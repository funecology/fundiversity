test_that("memoise options work fd_chull() & fd_chull_intersect() works", {
  skip_if_not_installed("memoise")

  expect_true(memoise::is.memoised(fd_chull_memoised))
  expect_true(memoise::is.memoised(fd_chull_intersect_memoised))

  data("traits_birds")
  ch   <- fd_chull_memoised(traits_birds)
  ch_i <- fd_chull_intersect_memoised(traits_birds)

  expect_true(memoise::has_cache(fd_chull_memoised)(traits_birds))
  expect_true(memoise::has_cache(fd_chull_intersect_memoised)(traits_birds))

  # Explicitly set up the memoise option to FALSE
  options(fundiversity.memoise = FALSE)
  expect_false(getOption("fundiversity.memoise", TRUE))

  fundiversity:::.onLoad(system.file(package = "fundiversity"), "fundiversity")
  expect_false(memoise::is.memoised(fd_chull_memoised))
  expect_false(memoise::is.memoised(fd_chull_intersect_memoised))

  # Reset the option to TRUE
  options(fundiversity.memoise = TRUE)
  expect_true(getOption("fundiversity.memoise", TRUE))

  fundiversity:::.onLoad(system.file(package = "fundiversity"), "fundiversity")
  expect_true(memoise::is.memoised(fd_chull_memoised))
  expect_true(memoise::is.memoised(fd_chull_intersect_memoised))

  ch2   <- fd_chull_memoised(traits_birds[2:5,])
  ch_i2 <- fd_chull_intersect_memoised(traits_birds[2:5,])

  expect_true(memoise::has_cache(fd_chull_memoised)(traits_birds[2:5,]))
  expect_true(
    memoise::has_cache(fd_chull_intersect_memoised)(traits_birds[2:5,])
  )

})
