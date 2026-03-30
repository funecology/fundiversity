# Compute Functional Divergence (FDiv)

This function computes Functional Divergence (FDiv) following Villéger
et al. (2008). NB: when a site contains no species FDiv is equal to 0.
If for a site there are less traits than species, then FDiv is equal to
`NaN`.

## Usage

``` r
fd_fdiv(traits, sp_com)
```

## Arguments

- traits:

  Trait matrix with species as rows and traits as columns. It has to
  contain exclusively numerical values. This can be either a `matrix`, a
  `data.frame`, or a
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  object.

- sp_com:

  Site-species matrix with sites as rows and species as columns if not
  provided, the function considers all species with equal abundance in a
  single site. This can be either a `matrix`, a `data.frame`, or a
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  object.

## Value

a data.frame with two columns:

- `site` the names of the sites as the row names of the input `sp_com`,

- `FDiv` the values of functional divergence at each site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: when a site contains no species FDiv is equal to 0. If for a site
there are less traits than species, then FDiv is equal to `NaN`.

## Details

By default, when loading fundiversity, the functions to compute convex
hulls are [memoised](https://en.wikipedia.org/wiki/Memoization) through
the `memoise` package if it is installed (their results are cached to
avoid recomputing the same functional volume twice). To deactivate this
behavior you can set the option `fundiversity.memoise` to `FALSE` by
running the following line: `options(fundiversity.memoise = FALSE)`. If
you use it interactively it will only affect your current session. Add
it to your script(s) or `.Rprofile` file to avoid toggling it each time.
By changing the option, the behavior will automatically change the next
time you run the function. **Note**: memoisation is only available when
the `memoise` package has been installed **and without
parallelization**, otherwise `fundiversity` will use unmemoised versions
of the functions. In other words, **memoization and parallelization are
mutually exclusive**.

## Parallelization

The computation of this function can be parallelized thanks to
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
To get more information on how to parallelize your computation please
refer to the parallelization vignette with:
[`vignette("fundiversity_1-parallel", package = "fundiversity")`](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md).

## References

Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
functional diversity indices for a multifaceted framework in functional
ecology, Ecology 89(8),
[doi:10.1890/07-1206.1](https://doi.org/10.1890/07-1206.1)

## Examples

``` r
data(traits_birds)
fd_fdiv(traits_birds)
#>   site      FDiv
#> 1   s1 0.7282172
```
