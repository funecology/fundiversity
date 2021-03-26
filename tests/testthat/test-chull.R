data("traits_birds")

test_that("memoise options works on fd_chull()", {
  skip_if_not_installed("memoise")

  if (getOption("fundiversity.memoise", TRUE)) {
    expect_true(memoise::is.memoised(fd_chull))

    ch <- fd_chull(traits_birds)

    expect_true(memoise::has_cache(fd_chull)(traits_birds))
  } else {
    expect_false(memoise::is.memoised(fd_chull))

    ch <- fd_chull(traits_birds)

    expect_false(memoise::has_cache(fd_chull)(traits_birds))
  }

})
