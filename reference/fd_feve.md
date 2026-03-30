# Compute Functional Evenness (FEve)

This function computes Functional Evenness (FEve) following Villéger et
al. (2008). NB: By definition FEve is equal to `NA` when the number of
species per site is strictly lower than 3.

## Usage

``` r
fd_feve(traits = NULL, sp_com, dist_matrix = NULL)
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

- dist_matrix:

  A dissimilarity matrix that can be provided instead of a trait
  data.frame (default: `NULL`). This can be either a `matrix`, a
  `data.frame`, or a
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  object.

## Value

a data.frame with two columns:

- `site` character column that contains site names based on input
  `sp_com` row names,

- `FEve` numeric column that contains FEve values corresponding to each
  site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: By definition FEve is equal to `NA` when the number of species per
site is strictly lower than 3.

## Parallelization

The computation of this function can be parallelized thanks to
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
To get more information on how to parallelize your computation please
refer to the parallelization vignette with:
[`vignette("fundiversity_1-parallel", package = "fundiversity")`](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md).

## References

Villéger, S., Mason, N.W.H., Mouillot, D., 2008. New Multidimensional
Functional Diversity Indices for a Multifaceted Framework in Functional
Ecology. Ecology 89, 2290–2301.
[doi:10.1890/07-1206.1](https://doi.org/10.1890/07-1206.1)

## Examples

``` r
data(traits_birds)
fd_feve(traits_birds)
#>   site      FEve
#> 1   s1 0.3743341
```
