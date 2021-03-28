data("traits_birds")

test_that("memoise options work on fd_chull_intersect()", {
  skip_if_not_installed("memoise")

  if (getOption("fundiversity.memoise", TRUE)) {
    expect_true(memoise::is.memoised(fd_chull_intersect))

    ch_i <- fd_chull_intersect(traits_birds, traits_birds)

    expect_true(
      memoise::has_cache(fd_chull_intersect)(traits_birds, traits_birds)
    )
  } else {
    expect_false(memoise::is.memoised(fd_chull_intersect))

    ch_i <- fd_chull_intersect(traits_birds, traits_birds)

    expect_false(
      memoise::has_cache(fd_chull_intersect)(traits_birds, traits_birds)
    )
  }

})
