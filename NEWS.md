# fundiversity 1.0.0

## Internal changes

* Unit tests for non-continuous traits for all functions

## Minor changes

* fundiversity functions now error when used with non-continuous trait data 
* `future_apply()` and `future_lapply()` calls in `fd_fdis()`, `fd_fdiv()`,
`fd_ric()`, `fd_fric_intersect()` and `fd_feve()` now use 
`future.globals = FALSE`, thus making the internal code less error-prone and
faster in parallel settings.

# fundiversity 0.2.1

## Internal changes

* Unit tests have been updated to work with testthat 3.1.0

# fundiversity 0.2.0

## Major changes

* There is a new function `fd_fric_intersect()` to compute the intersection of
convex hulls across pairs of sites.
* There is a new function `fd_fdis` to compute Functional Dispersion FDis.
* `fd_fdiv()`, `fd_feve()`,  `fd_fdis()`, `fd_fric()` can now be computed in
parallel with the [future](https://cran.r-project.org/package=future) framework. 
Please refer to the [parallelization vignette for more information](https://bisaloo.github.io/fundiversity/articles/parallel.html).
* The convex hull computation in `fd_fdiv()`, `fd_fric()`, and 
`fd_fric_intersect()` is now cached thanks to the 
[memoise](https://memoise.r-lib.org/) package to speed up repeated runs. This 
behaviour can be disabled by running `options(fundiversity.memoise = FALSE)` at
the start of your R session.

# fundiversity 0.0.1

* First CRAN release (01751f1) :tada:!
* Added a `NEWS.md` file to track changes to the package.
