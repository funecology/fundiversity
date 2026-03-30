# Intersection between convex hulls of pairs of sites

Compute volume of the intersection of the convex hulls of all pairs of
sites (including self-intersection, which corresponds to their convex
hull). Note that when standardizing convex hulls of intersections, this
function uses the convex hull of all provided traits, thus standardized
volume of self-intersection hulls can be lower than one. NB:
FRic_intersect is equal to `NA` when there are strictly less species in
one of the sites than the number of provided traits.

## Usage

``` r
fd_fric_intersect(traits, sp_com, stand = FALSE)
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

a data.frame with three columns:

- `first_site` the names of the first site used in the pair `sp_com`,

- `second_site` the names of the first site used in the pair,

- `FRic_intersect` the volume of the convex hulls intersection of each
  pair of site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: FRic_intersect is equal to `NA` when there are strictly less species
in one of the sites than the number of provided traits. Note that only
species with strictly different trait combinations are considered
unique, species that share the exact same trait values across all traits
are considered as one species.

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

Villéger S., Grenouillet G., Brosse S. (2013), Decomposing functional
\\\beta\\-diversity reveals that low functional \\\beta\\-diversity is
driven by low functional turnover in European fish assemblages, Global
Ecology and Biogeography, 22(6), 671–681.
[doi:10.1111/geb.12021](https://doi.org/10.1111/geb.12021) .

Zhao T., Villéger S., Cucherousset J. (2019). Accounting for
intraspecific diversity when examining relationships between non-native
species and functional diversity. Oecologia, 189(1), 171-183.
[doi:10.1007/s00442-018-4311-3](https://doi.org/10.1007/s00442-018-4311-3)
.

## See also

[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md),
[`geometry::intersectn()`](https://rdrr.io/pkg/geometry/man/intersectn.html),
[`geometry::convhulln()`](https://rdrr.io/pkg/geometry/man/convhulln.html)

## Examples

``` r
data(traits_birds)
fd_fric_intersect(traits_birds)
#>   first_site second_site FRic_intersect
#> 1         s1          s1       230967.7
```
