# Compute Functional Dispersion (FDis)

This function computes Functional Dispersion (FDis) following Laliberté
& Legendre (2010). NB: when a site contains no species FDis is equal to
0.

## Usage

``` r
fd_fdis(traits, sp_com)
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

- `FDis` the values of functional dispersion at each site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: when a site contains no species FDis is equal to 0.

## Parallelization

The computation of this function can be parallelized thanks to
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
To get more information on how to parallelize your computation please
refer to the parallelization vignette with:
[`vignette("fundiversity_1-parallel", package = "fundiversity")`](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md).

## References

Laliberté, E., & Legendre, P. (2010). A distance-based framework for
measuring functional diversity from multiple traits. Ecology, 91(1),
299–305. [doi:10.1890/08-2244.1](https://doi.org/10.1890/08-2244.1)

## Examples

``` r
data(traits_birds)
data(site_sp_birds)
fd_fdis(traits_birds, site_sp_birds)
#>        site      FDis
#> 1  elev_250 151.38851
#> 2  elev_500 153.79982
#> 3 elev_1000 161.57816
#> 4 elev_1500 144.30915
#> 5 elev_2000  76.69386
#> 6 elev_2500  78.44577
#> 7 elev_3000  88.25201
#> 8 elev_3500  68.29563
```
