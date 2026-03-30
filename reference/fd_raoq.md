# Compute Rao's entropy index (Rao's Q)

This function computes Rao's Quadratic Entropy following Rao (1982). NB:
Rao's quadratic entropy is 0 when there are no species in the site.

## Usage

``` r
fd_raoq(traits = NULL, sp_com, dist_matrix = NULL)
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

- `site` the names of the sites as the row names of the input `sp_com`,

- `Q` the values of Rao's quadratic entropy at each site.

If the `sp_com` argument is not provided or if `sp_com` doesn't have
rownames, arbitrary rownames `s1`, `s2`, `s3`, etc. will be used.

NB: Rao's quadratic entropy is 0 when there are no species in the site.

## References

Pavoine S., Dolédec S. (2005). The apportionment of quadratic entropy: a
useful alternative for partitioning diversity in ecological data.
Environmental and Ecological Statistics, 12(2), 125–138.
[doi:10.1007/s10651-005-1037-2](https://doi.org/10.1007/s10651-005-1037-2)

## Examples

``` r
data(traits_birds)
fd_raoq(traits_birds)
#>   site        Q
#> 1   s1 170.0519
```
