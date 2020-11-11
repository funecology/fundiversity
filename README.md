
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fundiversity

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of fundiversity is to provide a package to compute common
functional diversity indices. The package is built using clear, public
[design
principles](https://github.com/Bisaloo/fundiversity/wiki/Design-principles)
inspired from our own experience and user feedback. We also strive to
use the latest good practice in software and R package development.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Bisaloo/fundiversity")
```

## Example

You can compute functional diversity indices through the functions
`fd_fric()`, `fd_fdiv()`, and `fd_rao()`:

``` r
library(fundiversity)

# Get trait data included in the package
data("traits_birds")

# Compute Functional Richness of all birds included
fd_fric(traits_birds)
#>    site     FRic
#> s1   s1 230967.7
```

## Related Packages

  - [`FD`](https://cran.r-project.org/package=FD) historical package to
    compute functional diversity indices, but has not been update since
    August 2014;
  - [`hillR`](https://cran.r-project.org/package=hillR) can compute hill
    Numbers for taxonomic, functional, and phylogenetic diversity, but
    it does not include other functional diversity indices;
  - [`adiv`](https://cran.r-project.org/package=adiv) proposes various
    diversity indices adapted from Rao’s Quadratic Entropy but doesn’t
    include other functional diversity indices;
  - [`fundiv`](https://github.com/ibartomeus/fundiv), is a wrapper
    around the `FD` package but hasn’t been updated since 2016 and
    hasn’t been published on CRAN;

Read more about other packages that are similar to `fundiversity` on our
[dedicated wiki
page](https://github.com/Bisaloo/fundiversity/wiki/Similar-packages)
