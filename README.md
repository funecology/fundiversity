
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fundiversity

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/Bisaloo/fundiversity/workflows/R-CMD-check/badge.svg)](https://github.com/Bisaloo/fundiversity/actions)
[![Coverage
Status](https://codecov.io/gh/Bisaloo/fundiversity/branch/master/graph/badge.svg?token=HR4YH118VT)](https://codecov.io/gh/Bisaloo/fundiversity)
<!-- badges: end -->

`fundiversity` provides a lightweight package to compute common
functional diversity indices. The package is built using clear, public
[design
principles](https://github.com/Bisaloo/fundiversity/wiki/Design-principles)
inspired from our own experience and user feedback. We also strive to
use the latest good practice in software and R package development,
including minimal dependencies on external packages (currently
`fundiversity` only depends on `geometry`).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Bisaloo/fundiversity")
```

## Example

`fundiversity` lets you compute three classical functional diversity
indices: Functional Richness with `fd_fric()`, Functional Divergence
with `fd_fdiv()`, and Rao’s Quadratic Entropy with `fd_raoq()`. All
indices can be computed either using global trait data or at the
site-level:

``` r
library(fundiversity)

# Get trait data included in the package
data("traits_birds")

# Compute Functional Richness of all birds included
fd_fric(traits_birds)
#>   site     FRic
#> 1   s1 230967.7

# Compute Functional Divergence
fd_fdiv(traits_birds)
#>   site      FDiv
#> 1   s1 0.6011971

# Compute Rao's Quadratic Entropy
fd_raoq(traits_birds)
#>   site        Q
#> 1   s1 170.0519
```

To compute Rao’s Quadratic Entropy, the user can also provide a distance
matrix between species directly:

``` r
dist_traits_birds = as.matrix(dist(traits_birds))

fd_raoq(traits = NULL, dist_matrix = dist_traits_birds)
#>   site        Q
#> 1   s1 170.0519
```

## Related Packages

Several other packages exist that compute functional diversity indices,
we here mention some of them (but do not mention the wrappers around
these packages):

| Package Name                                           | Indices included                                            | Has tests | On GitHub | On CRAN (last updated)                                     |
| ------------------------------------------------------ | ----------------------------------------------------------- | --------- | --------- | ---------------------------------------------------------- |
| [`FD`](https://github.com/cran/FD)                     | FRic, FDiv, FDis, FEve, Rao’s QE, Functional Group Richness | :x:       | :x:       | ![](https://www.r-pkg.org/badges/last-release/FD)          |
| [`adiv`](https://github.com/cran/adiv)                 | Functional Entropy, Functional Redundancy                   | :x:       | :x:       | ![](https://www.r-pkg.org/badges/last-release/adiv)        |
| [`betapart`](https://github.com/cran/betapart)         | Functional β-diversity                                      | :x:       | :x:       | ![](https://www.r-pkg.org/badges/last-release/betapart)    |
| [`entropart`](https://github.com/EricMarcon/entropart) | Functional Entropy                                          | ✅         | ✅         | ![](https://www.r-pkg.org/badges/last-release/entropart)   |
| [`hillR`](https://github.com/daijiang/hillR)           | Functional Diversity Hill Number                            | ✅         | ✅         | ![](https://www.r-pkg.org/badges/last-release/hillR)       |
| [`vegan`](https://github.com/vegandevs/vegan)          | Only dendrogram-based FD (`treedive()`)                     | ✅         | ✅         | ![](https://www.r-pkg.org/badges/last-release/vegan)       |
| [`TPD`](https://github.com/cran/TPD)                   | FRic, FDiv, FEve but for probability distributions          | :x:       | :x:       | ![](https://www.r-pkg.org/badges/last-release/TPD)         |
| [`hypervolume`](https://github.com/cran/hypervolume)   | Hypervolume measure functional diversity (\~FRic)           | :x:       | ✅         | ![](https://www.r-pkg.org/badges/last-release/hypervolume) |
| [`BAT`](https://github.com/cran/BAT)                   | β-, Richness, divergence, and evenness with hypervolumes    | :x:       | ✅         | ![](https://www.r-pkg.org/badges/last-release/BAT)         |
