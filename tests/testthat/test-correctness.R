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
