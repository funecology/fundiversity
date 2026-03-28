# These tests are also exposed to users in the `fundiversity_3-correctness`
# vignette.
data_a <- matrix(byrow = TRUE, ncol = 2,
  c(
    0.0, 1.0,
    0.5, 1.0,
    1.0, 1.0,
    1.5, 1.0,
    2.0, 1.0,
    1.0, 0.0,
    1.0, 0.5,
    1.0, 1.5,
    1.0, 2.0
))
rownames(data_a) <- paste0("species", seq_len(nrow(data_a)))

l <- 1.5 # common species
s <- 0.5 # rare species

wb <- matrix(c(l, l, 1, l, l, s, s, s, s), nrow = 1)
colnames(wb) <- rownames(data_a)
rownames(wb) <- "s1"

l <- 2 # common species
s <- 1 # rare species

wc <- matrix(c(l, s, l, s, l, l, s, s, l), nrow = 1)
colnames(wc) <- rownames(data_a)
rownames(wc) <- "s1"

shift <- 1/(2*sqrt(2))

data_d <- matrix(c(
  1-shift, 1-shift,
  1-shift, 1+shift,
  1+shift, 1-shift,
  1+shift, 1+shift,
  1.00 , 0.00 ,
  2.00 , 1.00 ,
  1.00 , 2.00 ,
  0.00 , 1.00 ,
  1.00 , 1.00),
  byrow = TRUE,
  ncol = 2
)

data_e <- matrix(c(
  0.0, 1.0,
  0.5, 0.5,
  1.0, 1.0,
  1.5, 0.5,
  2.0, 1.0,
  1.0, 0.0,
  0.5, 1.5,
  1.5, 1.5,
  1.0, 2.0),
  byrow = TRUE,
  ncol = 2
)

test_that("theoretical expectations from figure 2a from Villeger2008 are met", {
  expect_equal(fd_fric(data_a)[["FRic"]], 2, tolerance = 1e-3)
  expect_identical(fd_feve(data_a)[["FEve"]], 1)
  expect_equal(fd_fdiv(data_a)[["FDiv"]], 0.692, tolerance = 1e-3)
})

test_that("theoretical expectations from figure 2b from Villeger2008 are met", {
  expect_identical(fd_fric(data_a, wb), fd_fric(data_a))
  expect_equal(fd_feve(data_a, wb)[["FEve"]], 0.778, tolerance = 1e-3)
  expect_identical(fd_fdiv(data_a, wb), fd_fdiv(data_a))
})

test_that("theoretical expectations from figure 2c from Villeger2008 are met", {
  expect_identical(fd_fric(data_a, wc), fd_fric(data_a))
  expect_identical(fd_feve(data_a, wc), fd_feve(data_a))
  expect_equal(fd_fdiv(data_a, wc)[["FDiv"]], 0.714, tolerance = 1e-3)
})


test_that("theoretical expectations from figure 2d from Villeger2008 are met", {
  expect_identical(fd_fric(data_d), fd_fric(data_a))
  expect_equal(fd_feve(data_d)[["FEve"]], 0.891, tolerance = 1e-3)
  expect_equal(fd_fdiv(data_d), fd_fdiv(data_a), tolerance = 1e-8)
})

test_that("theoretical expectations from figure 2e from Villeger2008 are met", {
  expect_identical(fd_fric(data_e), fd_fric(data_a))
  expect_identical(fd_feve(data_e), fd_feve(data_a))
  expect_identical(fd_fdiv(data_e)[["FDiv"]], 0.78, tolerance = 1e-3)
})

test_that("theoretical expectation from appendix C from Laliberté2010 are met", {
  # https://doi.org/10.6084/m9.figshare.3544118
  
  # Criterion 1
  traits <- matrix(
    c(1, 1, 1, 1), 
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(10, 10, 10, 10), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"
  
  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 0)

  traits <- matrix(
    c(0, 1e-2, 1, 1e2), 
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 37.3625, tolerance = 1e-3)

  # Criterion 2
  traits <- matrix(
    c(2, 4, 6, 8),
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(1, 1, 1, 1), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 2)

  traits <- matrix(
    c(0.1, 1, 10, 100),
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 36.1125)

  # Criterion 3
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(1, 100, 100, 1), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 4.95)

  sp_com <- matrix(c(10, 10, 10, 10), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 36.1125)

  # Criterion 4
  sp_com <- matrix(c(0.1, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 16.23, tolerance = 1e-4)

  sp_com <- matrix(c(1, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 16.35, tolerance = 1e-4)

  # Criterion 5
  sp_com <- matrix(c(0, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(fd_fdis(traits, sp_com)[["FDis"]], 16.22, tolerance = 1e-3)

  expect_equal(fd_fdis(traits[-1, ], sp_com[, -1, drop = FALSE])[["FDis"]], 16.2162, tolerance = 1e-4)

  # Criterion 6
  traits <- matrix(
    c(0, 0.01, 0.1, 1), 
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(1, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(
    fd_fdis(traits, sp_com)[["FDis"]], 
    0.1635,
    tolerance = 1e-4
  )

  # Criterion 7
  traits <- matrix(
    c(2, 4, 6, 8),
    ncol = 1
  )
  rev_traits <- matrix(
    c(8, 6, 4, 2),
    ncol = 1
  )
  rownames(rev_traits) <- rownames(traits) <- paste0("species", seq_len(nrow(traits)))

  expect_equal(
    fd_fdis(traits, sp_com)[["FDis"]],
    fd_fdis(rev_traits, sp_com)[["FDis"]]
  )

  # Criterion 8
  traits <- matrix(
    c(0.1, 1, 10, 100),
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(1, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  expect_equal(
    fd_fdis(traits, sp_com)[["FDis"]],
    fd_fdis(traits, sp_com * 100)[["FDis"]]
  )

  # Criterion 9
  traits <- matrix(
    c(0.1, 1, 10, 100),
    ncol = 1
  )
  rownames(traits) <- paste0("species", seq_len(nrow(traits)))
  sp_com <- matrix(c(1, 10, 100, 1000), nrow = 1)
  colnames(sp_com) <- rownames(traits)
  rownames(sp_com) <- "s1"

  traits_split <- matrix(
    c(0.1, 1, 10, 100, 100),
    ncol = 1
  )
  rownames(traits_split) <- paste0("species", seq_len(nrow(traits_split)))
  sp_com_split <- matrix(c(1, 10, 100, 500, 500), nrow = 1)
  colnames(sp_com_split) <- rownames(traits_split)
  rownames(sp_com_split) <- "s1"
  expect_equal(
    fd_fdis(traits, sp_com)[["FDis"]],
    fd_fdis(traits_split, sp_com_split)[["FDis"]]
  )
})