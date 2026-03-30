# Changelog

## fundiversity (development version)

- [`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md)
  is now ~20x faster, removing the need for parallel processing.

## fundiversity 1.1.0

### Minor changes

- Change vignette names and indexing so that they are ordered similarly
  on the pkgdown website, on CRAN, and using `vignette`. Vignettes are
  renamed `fundiversity_X-topic.Rmd` but the overview vignette
  `fundiversity.Rmd`.
- Fully replace all URLs still pointing to
  <https://github.com/bisaloo/fundiversity> to
  <https://github.com/funecology/fundiversity>
- Update documentation of `fd_*()` functions for edge cases and
  arguments.
- Fix unuseful argument in `remove_species_without_traits()` that would
  display `FALSE` at the end of each message.
- Update parallel vignette with list of parallelizable function at the
  top and explain that vignette is pre-computed.
- Update performance/benchmark vignette to reflect actually ran
  benchmark in manuscript.

### Bug fixes

- Fix a bug in the computation of
  [`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md)
  because of a misplaced square. **NB**: All FDis computation done with
  it to this day were wrong.

## fundiversity 1.0.0

CRAN release: 2022-08-25

### Internal changes

- Unit tests for non-continuous traits for all functions.
- Correct all URLs of the package

### Minor changes

- fundiversity functions now error when used with non-continuous trait
  data
- `future_apply()` and `future_lapply()` calls in
  [`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md),
  [`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md),
  `fd_ric()`,
  [`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
  and
  [`fd_feve()`](https://funecology.github.io/fundiversity/reference/fd_feve.md)
  now use `future.globals = FALSE`, thus making the internal code less
  error-prone and faster in parallel settings.

### Major changes

- Add a vignette on numerical correctness named `correctness`,
  accessible with `vignette("correctness", package = "fundiversity")`.

## fundiversity 0.2.1

CRAN release: 2021-09-21

### Internal changes

- Unit tests have been updated to work with testthat 3.1.0

## fundiversity 0.2.0

CRAN release: 2021-05-14

### Major changes

- There is a new function
  [`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
  to compute the intersection of convex hulls across pairs of sites.
- There is a new function `fd_fdis` to compute Functional Dispersion
  FDis.
- [`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md),
  [`fd_feve()`](https://funecology.github.io/fundiversity/reference/fd_feve.md),
  [`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md),
  [`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
  can now be computed in parallel with the
  [future](https://cran.r-project.org/package=future) framework. Please
  refer to the [parallelization vignette for more
  information](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.html).
- The convex hull computation in
  [`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md),
  [`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md),
  and
  [`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
  is now cached thanks to the [memoise](https://memoise.r-lib.org/)
  package to speed up repeated runs. This behavior can be disabled by
  running `options(fundiversity.memoise = FALSE)` at the start of your R
  session.

## fundiversity 0.0.1

CRAN release: 2021-02-15

- First CRAN release (01751f1) 🎉!
- Added a `NEWS.md` file to track changes to the package.
