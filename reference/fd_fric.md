# Compute Functional Richness (FRic)

Functional Richness is computed as the volume of the convex hull from
all included traits following Villéger et al. (2008). NB: FRic is equal
to `NA` when there are strictly less species in a site than the number
of provided traits.

## Usage

``` r
fd_fric(traits, sp_com, stand = FALSE)
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

- stand:

  a boolean indicating whether to standardize FRic values over the
  observed maximum over all species (default: `FALSE`). This scales FRic
  between 0 and 1. **NB**: The maximum FRic values only considers
  species that are present in **both** site-species and trait matrices.
  If you want to consider species that are absent in the site-species
  matrix, add corresponding columns of 0s.

## Value

a data.frame with two columns:

- `site` the names of the sites as the row names of the input `sp_com`,

- `FRic` the values of functional richness at each site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: FRic is equal to `NA` when there are strictly less species in a site
than the number of provided traits. Note that only species with strictly
different trait combinations are considered unique, species that share
the exact same trait values across all traits are considered as one
species.

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

Cornwell W. K., Schwilk D. W., Ackerly D. D. (2006), A trait-based test
for habitat filtering; convex hull volume, Ecology 84(6),
[doi:10.1890/0012-9658(2006)87\[1465:ATTFHF\]2.0.CO;2](https://doi.org/10.1890/0012-9658%282006%2987%5B1465%3AATTFHF%5D2.0.CO%3B2)

Villéger, S., Mason, N.W.H., Mouillot, D., 2008. New Multidimensional
Functional Diversity Indices for a Multifaceted Framework in Functional
Ecology. Ecology 89, 2290–2301.
[doi:10.1890/07-1206.1](https://doi.org/10.1890/07-1206.1)

## Examples

``` r
data(traits_birds)
fd_fric(traits_birds)
#>   site     FRic
#> 1   s1 230967.7
```
