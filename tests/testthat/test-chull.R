test_that("memoised fd_chull works", {
  skip_if_not_installed("memoise")

  expect_true(memoise::is.memoised(fd_chull))

  data("traits_birds")
  ch <- fd_chull(traits_birds)

  expect_true(memoise::has_cache(fd_chull)(traits_birds))
})
