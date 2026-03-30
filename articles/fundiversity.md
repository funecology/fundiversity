# 1. Overview of how to use fundiversity

`fundiversity` lets you compute functional diversity indices. Currently
it can compute five indices:

- functional richness (FRic),
- functional volume intersections (FRic_intersect),
- functional divergence (FDiv),
- functional evenness (FEve),
- functional dispersion (FDis)
- Rao’s quadratic entropy (Q).

This vignette will introduce you to the data needed as well as how to
compute and interpret each index. We made sure the computations of these
indices are correct based on a test dataset as specified in the
[correctness
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_3-correctness.md).

``` r
library("fundiversity")
```

## Required data

To compute functional diversity indices, you will need at least a
dataset describing species traits, i.e. species characteristics. Note
that we here talk about species but the reasoning could apply on
whatever unit you’re interested in whether it’s individual organisms,
ecological plots, or even entire ecosystems. The traits are the features
that describe these units.

`fundiversity` comes with one example trait dataset. The dataset comes
from Nowak et al. ([2019b](#ref-Nowak_Projecting_2019)) and describe the
traits of birds and plants along a tropical gradient ([Nowak et al.
2019a](#ref-Nowak_Data_2019)). You can see the datasets available in
`fundiversity` using the [`data()`](https://rdrr.io/r/utils/data.html)
function:

``` r
data(package = "fundiversity")
```

To load them use their names into the
[`data()`](https://rdrr.io/r/utils/data.html) function:

``` r
data("traits_birds", package = "fundiversity")

head(traits_birds)
#>                      Bill.width..mm. Bill.length..mm. Kipp.s.index Bodymass..g.
#> Aburria_aburri                 18.35            35.48         0.18       1407.5
#> Amazona_farinosa               26.50            38.81         0.29        626.0
#> Amazona_mercenaria             17.51            26.30         0.33        340.0
#> Amazona_ochrocephala           20.17            31.40         0.26        440.0
#> Ampelioides_tschudii           16.53            24.58         0.24         78.4
#> Ampelion_rufaxilla             16.97            21.89         0.28         73.9

data("traits_plants", package = "fundiversity")

head(traits_plants)
#>                        Fruit.length..mm. Fruit.width..mm. Plant.height..m.
#> Abuta_grandifolia                  17.48             9.38             6.00
#> Alchornea_latifolia                 6.82             8.23            11.53
#> Alchornea_triplinervia              8.18            10.26            14.72
#> Allophylus_punctatus               13.58            12.49             4.40
#> Alnus_acuminata                    19.32            11.48            17.54
#> Aniba_guianensis                   15.65            13.43             8.00
#>                        Crop.mass..g.
#> Abuta_grandifolia             341.25
#> Alchornea_latifolia          1290.48
#> Alchornea_triplinervia       1156.67
#> Allophylus_punctatus           33.34
#> Alnus_acuminata              5009.33
#> Aniba_guianensis               25.50
```

Note that in these datasets the **species** are shown in **rows**, with
species names as row names, and **traits** are in **columns**.

Functional diversity indices are generally computed at different
locations that we hereafter call sites. We thus need a description of
which species is in which site in the form of a site-species matrix.
Again, we’re calling it a site-species matrix but the granularity of
both your “species” and “site” units can vary depending on what you want
to compute functional diversity on.

`fundiversity` contains the corresponding site-species matrices to the
above-mentioned trait dataset ([Nowak et al.
2019a](#ref-Nowak_Data_2019)):

``` r
# Site-species matrix for birds
data("site_sp_birds", package = "fundiversity")

head(site_sp_birds)[, 1:5]
#>           Aburria_aburri Amazona_farinosa Amazona_mercenaria
#> elev_250               0                1                  0
#> elev_500               0                1                  0
#> elev_1000              1                1                  1
#> elev_1500              1                0                  1
#> elev_2000              0                0                  1
#> elev_2500              0                0                  1
#>           Amazona_ochrocephala Ampelioides_tschudii
#> elev_250                     1                    0
#> elev_500                     1                    0
#> elev_1000                    0                    1
#> elev_1500                    0                    0
#> elev_2000                    0                    0
#> elev_2500                    0                    0

# Site-species matrix for plants
data("site_sp_plants", package = "fundiversity")

head(site_sp_plants)[, 1:5]
#>           Abuta_grandifolia Alchornea_latifolia Alchornea_triplinervia
#> elev_250                  1                   1                      1
#> elev_500                  1                   1                      1
#> elev_1000                 0                   0                      1
#> elev_1500                 0                   0                      1
#> elev_2000                 0                   0                      1
#> elev_2500                 0                   0                      1
#>           Allophylus_punctatus Alnus_acuminata
#> elev_250                     1               1
#> elev_500                     1               1
#> elev_1000                    1               1
#> elev_1500                    1               1
#> elev_2000                    1               1
#> elev_2500                    1               1
```

The site species matrix represent the presence of a given **species**
(in **column**) in a given **site** (in **row**), similar to the format
used in the [`vegan`](https://cran.r-project.org/package=vegan) package.
Here the site-species matrix contains only 0 (absence) and 1 (presence),
but `fundiversity` can also use matrices that contain abundances for
some functional diversity indices (FDiv and Q).

To ensure the good computation of functional diversity indices, at least
some of species names (row names) in the trait data need to be in to the
column names of the site species matrix:

``` r
# Fewer species in trait dataset than species in the site-species matrix
fd_fric(traits_birds[2:217,], site_sp_birds)
#> Differing number of species between trait dataset and site-species matrix
#> Taking subset of species
#>        site       FRic
#> 1  elev_250 171543.730
#> 2  elev_500 185612.548
#> 3 elev_1000 109615.330
#> 4 elev_1500  63992.817
#> 5 elev_2000  20065.764
#> 6 elev_2500  18301.176
#> 7 elev_3000  17530.651
#> 8 elev_3500   3708.735

# Fewer species in the site-species matrix than in the traits
fd_fric(traits_birds, site_sp_birds[, 1:60])
#> Differing number of species between trait dataset and site-species matrix
#> Taking subset of species
#>        site        FRic
#> 1  elev_250 18963.31311
#> 2  elev_500 18963.31311
#> 3 elev_1000 38586.75398
#> 4 elev_1500 38114.26828
#> 5 elev_2000  5888.93690
#> 6 elev_2500  5256.70628
#> 7 elev_3000  2710.81803
#> 8 elev_3500    88.11684

# No species in common between both dataset
fd_fric(traits_birds[1:5,], site_sp_birds[, 6:10])
#> Error:
#> ! No species in common found between trait dataset and site-species matrix
```

## Functional Richness (FRic) - `fd_fric()`

Functional Richness (FRic) represents the total amount of functional
space filed by a community in a dataset ([Villéger, Mason, and Mouillot
2008](#ref-Villeger_New_2008)). You can compute FRic in `fundiversity`
using the
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
function.

For a single trait range FRic is the range of trait observed in the
dataset:

``` r
# Range of bill width in the birds dataset
diff(range(traits_birds[, "Bill.width..mm."]))
#> [1] 33.64

# Using fundiversity::fd_fric()
fd_fric(traits_birds)
#>   site     FRic
#> 1   s1 230967.7
```

The first column `site` describes the site on which FRic has been
computed while the `FRic` column contains the computed FRic values. If
no site-species matrix has been provided the site is named by default
`s1`.

For multiple traits, FRic can be thought as a multi-dimensional range
which is computed as the convex hull volume of the considered species
([Villéger, Mason, and Mouillot 2008](#ref-Villeger_New_2008)):

``` r
fd_fric(traits_birds)
#>   site     FRic
#> 1   s1 230967.7
```

If you provide only a trait dataset without specifying site-species
matrix
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
computes FRic on the full trait dataset. You can compute FRic values for
different sites by providing both a trait dataset and a site-species
matrix to
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md):

``` r
fd_fric(traits_birds, site_sp_birds)
#>        site       FRic
#> 1  elev_250 171543.730
#> 2  elev_500 185612.548
#> 3 elev_1000 112600.176
#> 4 elev_1500  66142.748
#> 5 elev_2000  20065.764
#> 6 elev_2500  18301.176
#> 7 elev_3000  17530.651
#> 8 elev_3500   3708.735
```

Because the convex hull volume depends on the number and the units of
the traits used, it is difficult to compare across datasets, that is why
it has been suggested to standardize its value by the total volume
comprising all species in the dataset ([Villéger, Mason, and Mouillot
2008](#ref-Villeger_New_2008)):

``` r
fd_fric(traits_birds, stand = TRUE)
#>   site FRic
#> 1   s1    1
```

The newly computed FRic values will then be comprised between 0 and 1.
It is especially useful when comparing different sites:

``` r
fd_fric(traits_birds, site_sp_birds, stand = TRUE)
#>        site       FRic
#> 1  elev_250 0.74271733
#> 2  elev_500 0.80362981
#> 3 elev_1000 0.48751477
#> 4 elev_1500 0.28637225
#> 5 elev_2000 0.08687692
#> 6 elev_2500 0.07923694
#> 7 elev_3000 0.07590087
#> 8 elev_3500 0.01605737
```

Each row gives the standardized FRic values of each site.

**Parallelization**. The computation of this function can be
parallelized thanks to the `future` package. Refer to the
[parallelization
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md)
to get more information about how to do so.

**Memoization**. By default, when loading `fundiversity`, the functions
to compute convex hulls are
[memoised](https://en.wikipedia.org/wiki/Memoization) through the
`memoise` package if it is installed (their results are cached to avoid
recomputing the same functional volume twice). It means repeated call to
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
with the same arguments won’t be recomputed but recovered from cache. To
deactivate this behavior you can set the option `fundiversity.memoise`
to `FALSE` by running the following line:
`options(fundiversity.memoise = FALSE)`. By changing the option, the
behavior will automatically change the next time you run the function.
Add it to your script(s) or `.Rprofile` file to avoid toggling it each
time. By changing the option, the behavior will automatically change the
next time you run the function. **Important note**: memoisation is only
available when the `memoise` package has been installed **and without
parallelization**, otherwise `fundiversity` will use unmemoised versions
of the functions.

## Functional volume intersect (FRic_intersect) - `fd_fric_intersect()`

Sometimes you’re interested in the shared functional volumes between
pairs of sites more than in the functional volumes of each site
separately. fundiversity provides the
[`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
function for this exact use case.

It follows the same interface as
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
with similar named arguments:

``` r
fd_fric_intersect(traits_birds)
#>   first_site second_site FRic_intersect
#> 1         s1          s1       230967.7
```

[`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
computes the shared functional volumes between each pair of sites,
including self-intersection which correspond to the functional volume of
each site. Similarly to
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
if no site-species data is provided,
[`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
considers a site that contains all species from the trait dataset.

``` r
fd_fric_intersect(traits_birds, site_sp_birds[1:2,])
#>   first_site second_site FRic_intersect
#> 1   elev_250    elev_500       171532.6
#> 2   elev_250    elev_250       171543.7
#> 3   elev_500    elev_500       185612.5
```

The output is a data.frame where the two first columns (`first_site` and
`second_site`) define the sites on which the intersection is computed,
the third column (`FRic_intersect`) contains the volume of the
intersection.

Similarly to
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
the intersections volumes can be standardized:

``` r
fd_fric_intersect(traits_birds, site_sp_birds[1:2,], stand = TRUE)
#>   first_site second_site FRic_intersect
#> 1   elev_250    elev_500      0.7426689
#> 2   elev_250    elev_250      0.7427173
#> 3   elev_500    elev_500      0.8036298
```

Note that when standardizing the volumes, the behavior is similar to
that of
[`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
which means the function considers the total volume occupied by provided
trait values, even if they are absent from all sites, this can lead to
standardized self-intersection volumes lower than one.

**Parallelization**. The computation of this function can be
parallelized thanks to the `future` package. Refer to the
[parallelization
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md)
to get more information about how to do so.

**Memoization**. By default, when loading `fundiversity`, the functions
to compute convex hulls are
[memoised](https://en.wikipedia.org/wiki/Memoization) through the
`memoise` package if it is installed. It means that repeated calls to
[`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md)
with similar arguments won’t be recomputed each time but recovered from
memory. To deactivate this behavior you can set the option
`fundiversity.memoise` to `FALSE` by running the following line:
`options(fundiversity.memoise = FALSE)`. By changing the option, the
behavior will automatically change the next time you run the function.
Add it to your script(s) or `.Rprofile` file to avoid toggling it each
time. **Important note**: memoisation is only available when the
`memoise` package has been installed **and without parallelization**,
otherwise `fundiversity` will use unmemoised versions of the functions.

## Functional Divergence (FDiv) - `fd_fdiv()`

Functional Divergence (FDiv) represents how abundance is spread along
the different traits ([Villéger, Mason, and Mouillot
2008](#ref-Villeger_New_2008)). When a species with extreme trait values
has the highest abundance, then functional divergence is high.

Use the
[`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md)
function to compute functional divergence:

``` r
# One-dimension FDiv
fd_fdiv(traits_birds[, 1, drop = FALSE])
#>   site      FDiv
#> 1   s1 0.7490732

# Multiple dimension FDiv
fd_fdiv(traits_birds)
#>   site      FDiv
#> 1   s1 0.7282172
```

When no site-species matrix is provided, FDiv is computed by default
considering all the species together. If you provide a site-species
matrix, then FDiv is computed across all sites:

``` r
fd_fdiv(traits_birds, site_sp_birds)
#>        site      FDiv
#> 1  elev_250 0.6847251
#> 2  elev_500 0.6937866
#> 3 elev_1000 0.7056772
#> 4 elev_1500 0.7269801
#> 5 elev_2000 0.7509511
#> 6 elev_2500 0.6985280
#> 7 elev_3000 0.6627204
#> 8 elev_3500 0.6422068
```

Similarly to FRic, if the included species differ between the
site-species matrix and the trait dataset,
[`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md)
will take the common subset of species.

The computation of this function can be parallelized thanks to the
`future` package. Refer to the [parallelization
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md)
to get more information about how to do so.

## Functional Evenness (FEve) - `fd_feve()`

Functional Evenness (FEve) describes the regularity of the distribution
of species (and their abundances) in trait space ([Villéger, Mason, and
Mouillot 2008](#ref-Villeger_New_2008)). FEve is bounded between 0
and 1. FEve is close to 0 when most species (and abundances) are tightly
packed in a portion of the trait space while it is close to 1 if species
are regularly spread (with even abundances) along the trait space.

Use the
[`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md)
function to compute functional divergence:

``` r
# One-dimension FEve
fd_feve(traits_birds[, 1, drop = FALSE])
#>   site      FEve
#> 1   s1 0.4454885

# Multiple dimension FEve
fd_feve(traits_birds)
#>   site      FEve
#> 1   s1 0.3743341
```

When no site-species matrix is provided, FEve is computed by default
considering all the species together. If you provide a site-species
matrix, then FEve is computed across all sites:

``` r
fd_feve(traits_birds, site_sp_birds)
#>        site      FEve
#> 1  elev_250 0.3841202
#> 2  elev_500 0.3846186
#> 3 elev_1000 0.3426688
#> 4 elev_1500 0.2965585
#> 5 elev_2000 0.3523994
#> 6 elev_2500 0.3552671
#> 7 elev_3000 0.3492529
#> 8 elev_3500 0.4222442
```

Similarly to FRic, if the included species differ between the
site-species matrix and the trait dataset,
[`fd_feve()`](https://funecology.github.io/fundiversity/reference/fd_feve.md)
will take the common subset of species.

The computation of this function can be parallelized thanks to the
`future` package. Refer to the [parallelization
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md)
to get more information about how to do so.

**Memoization**. By default, when loading `fundiversity`, the functions
to compute convex hulls are
[memoised](https://en.wikipedia.org/wiki/Memoization) through the
`memoise` package if it is installed (their results are cached to avoid
recomputing the same functional volume twice). It means repeated call to
[`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md)
with the same arguments won’t be recomputed but recovered from cache. To
deactivate this behavior you can set the option `fundiversity.memoise`
to `FALSE` by running the following line:
`options(fundiversity.memoise = FALSE)`. By changing the option, the
behavior will automatically change the next time you run the function.
Add it to your script(s) or `.Rprofile` file to avoid toggling it each
time. By changing the option, the behavior will automatically change the
next time you run the function. **Important note**: memoisation is only
available when the `memoise` package has been installed **and without
parallelization**, otherwise `fundiversity` will use unmemoised versions
of the functions.

## Functional Dispersion (FDis) - `fd_fdis()`

Functional Dispersion reflects changes in the abundance-weighted
deviation of species trait values from the center of the functional
space.

You can compute Functional Dispersion (FDis) using the
[`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md)
function by providing a trait dataset:

``` r
fd_fdis(traits_birds)
#>   site     FDis
#> 1   s1 133.3902
```

If you don’t provide a site-species matrix,
[`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md)
considers all species provided in the trait dataset present at equal
abundances in the same site. You can also provide a site-species matrix
to compute FDis at different sites:

``` r
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

The computation of this function can be parallelized thanks to the
`future` package. Refer to the [parallelization
vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.md)
to get more information about how to do so.

## Rao’s Quadratic Entropy (Q) - `fd_raoq()`

Rao’s Quadratic entropy assesses the multi-dimensional divergence in
trait space ([Rao 1982](#ref-Rao_Diversity_1982)). It is the
abundance-weighted variance of the trait dissimilarities between all
species pairs.

You can compute Rao’s Quadratic entropy (Q) using the
[`fd_raoq()`](https://funecology.github.io/fundiversity/reference/fd_raoq.md)
function by providing a trait dataset:

``` r
fd_raoq(traits_birds)
#>   site        Q
#> 1   s1 170.0519
```

If you don’t provide a site-species matrix,
[`fd_raoq()`](https://funecology.github.io/fundiversity/reference/fd_raoq.md)
considers all species provided in the trait dataset present at equal
abundances in the same site. You can also provide a site-species matrix
to compute Q at different sites:

``` r
fd_raoq(traits_birds, site_sp_birds)
#>        site         Q
#> 1  elev_250 194.78095
#> 2  elev_500 197.08184
#> 3 elev_1000 200.52231
#> 4 elev_1500 178.24801
#> 5 elev_2000  97.32416
#> 6 elev_2500 102.22461
#> 7 elev_3000 113.22049
#> 8 elev_3500  87.04750
```

Because the computation of Rao’s quadratic entropy requires
dissimilarities between all pair of species in the dataset, if you
provide a trait dataset
[`fd_raoq()`](https://funecology.github.io/fundiversity/reference/fd_raoq.md),
the function will compute the Euclidean distance between all pairs of
species. If you wish to directly provide species dissimilarities, you
can do so through the `dist_matrix` argument:

``` r
# Compute dissimilarity between species with the Manhattan distance
trait_dissim <- dist(traits_birds, method = "manhattan")

fd_raoq(dist_matrix = trait_dissim)
#>   site       Q
#> 1   s1 190.589

fd_raoq(sp_com = site_sp_birds, dist_matrix = as.matrix(trait_dissim))
#>        site        Q
#> 1  elev_250 218.3636
#> 2  elev_500 220.8257
#> 3 elev_1000 220.0048
#> 4 elev_1500 196.6785
#> 5 elev_2000 112.6211
#> 6 elev_2500 117.6497
#> 7 elev_3000 127.5911
#> 8 elev_3500 104.8981
```

NB: if you want to provide both a site-species matrix and a trait
dissimilarity matrix please specify explicitly the arguments names.

## Large site-species data / sparse matrices

[Sparse matrices](https://en.wikipedia.org/wiki/Sparse_matrix) are
memory efficient ways of storing matrix object that contains many zeros.
`fundiversity` is fully compatible with sparse matrices through the
[`Matrix` package](https://cran.r-project.org/package=Matrix). They can
be used to encode site-species information or distance matrices.

Provide `Matrix` objects as inputs of the indices function
`fundiversity`, they will transparently use them for efficient
computation.

``` r
# Convert site-species matrix to sparse matrix
sparse_site_sp <- Matrix::Matrix(site_sp_birds, sparse = TRUE)

fd_raoq(traits_birds, site_sp_birds)
#>        site         Q
#> 1  elev_250 194.78095
#> 2  elev_500 197.08184
#> 3 elev_1000 200.52231
#> 4 elev_1500 178.24801
#> 5 elev_2000  97.32416
#> 6 elev_2500 102.22461
#> 7 elev_3000 113.22049
#> 8 elev_3500  87.04750
```

## Standardizing trait data

`fundiversity` does not perform any transformation on the input trait or
dissimilarity data. In
[`fd_raoq()`](https://funecology.github.io/fundiversity/reference/fd_raoq.md)
if you provide only continuous trait data then the function will attempt
computing Euclidean distance between the species.

In order to get comparable functional diversity indices you can
standardize the trait data. One option would be to consider the
[`scale()`](https://rdrr.io/r/base/scale.html) function to scale each
continuous trait with a mean of zero and a standard deviation of one
(z-score). Each trait will then have the same importance when computing
functional diversity indices:

``` r
traits_birds_sc <- scale(traits_birds)
summary(traits_birds_sc)
#>  Bill.width..mm.   Bill.length..mm.    Kipp.s.index      Bodymass..g.     
#>  Min.   :-1.1854   Min.   :-0.78608   Min.   :-1.8545   Min.   :-0.49301  
#>  1st Qu.:-0.7330   1st Qu.:-0.52857   1st Qu.:-0.7432   1st Qu.:-0.44701  
#>  Median :-0.2974   Median :-0.28257   Median :-0.2492   Median :-0.34440  
#>  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000  
#>  3rd Qu.: 0.4157   3rd Qu.: 0.09482   3rd Qu.: 0.3682   3rd Qu.:-0.03999  
#>  Max.   : 3.9738   Max.   : 7.20531   Max.   : 3.3317   Max.   : 5.79396

# Unscaled
fd_fric(traits_birds)
#>   site     FRic
#> 1   s1 230967.7

# Scaled
fd_fric(traits_birds_sc)
#>   site    FRic
#> 1   s1 88.9286
```

Another solution to make trait comparable is to scale them between 0 and
1 by scaling each trait by its maximum and minimum values:

``` r
min_values <- as.numeric(lapply(as.data.frame(traits_birds), min))
max_values <- as.numeric(lapply(as.data.frame(traits_birds), max))

traits_birds_minmax <- apply(traits_birds, 1, function(x) {
  (x - min_values)/(max_values - min_values)
})
traits_birds_minmax <- t(traits_birds_minmax)
summary(traits_birds_minmax)
#>  Bill.width..mm.   Bill.length..mm.   Kipp.s.index     Bodymass..g.     
#>  Min.   :0.00000   Min.   :0.00000   Min.   :0.0000   Min.   :0.000000  
#>  1st Qu.:0.08769   1st Qu.:0.03222   1st Qu.:0.2143   1st Qu.:0.007317  
#>  Median :0.17212   Median :0.06301   Median :0.3095   Median :0.023637  
#>  Mean   :0.22977   Mean   :0.09837   Mean   :0.3576   Mean   :0.078418  
#>  3rd Qu.:0.31034   3rd Qu.:0.11023   3rd Qu.:0.4286   3rd Qu.:0.072057  
#>  Max.   :1.00000   Max.   :1.00000   Max.   :1.0000   Max.   :1.000000
```

There are several other options available to standardize trait values,
reviewed in Leps et al. ([2006](#ref-Leps_Quantifying_2006)).

If not all the traits you use are continuous, refer to the next section,
which suggests ways of computing functional diversity indices with
non-continuous traits.

## Non-continuous traits?

Do not panic. You can still compute the above-mentioned functional
diversity indices. However, as all indices need continuous descriptors
for all considered species, you need to transform the non-continuous
trait data into a continuous form. The general idea is to obtain from
the trait table a table of quantitative descriptions by defining
specific dissimilarity and projecting species dissimilarities onto
quantitative space using Principal Coordinates Analysis (PCoA). The
framework is fully described in Maire et al.
([2015](#ref-Maire_How_2015)).

To compute dissimilarity with non-continuous traits you can user Gower’s
distance ([Gower 1971](#ref-Gower_general_1971)) or its following
adaptations ([Pavoine et al. 2009](#ref-Pavoine_challenge_2009); [Podani
1999](#ref-Podani_Extending_1999)). You can use the following functions:
[`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html),
`FD::gowdis()`, `ade4::dist.ktab()`, or
[`vegan::vegdist()`](https://vegandevs.github.io/vegan/reference/vegdist.html).

Then you can project these dissimilarities with Principal Coordinates
using `ape::pcoa()` for example. You can then select the first
dimensions that explains the most variance and use theses as the input
“traits” to compute functional diversity indices.

## Missing values in traits?

Sometimes, some of the trait values can be missing for some species in
your dataset. Because `fundiversity` does not want to make assumptions
without telling you, by default it **drops** the species data for which
the trait is missing.

If you want to use data with missing values you can use dissimilarity
metrics that accept missing trait values such as some of the methods
specified in
[`vegan::vegdist()`](https://vegandevs.github.io/vegan/reference/vegdist.html).

Another solution, would be to impute the missing trait value to fill it.
Many imputation methods exists and trait imputation is out of the scope
of `fundiversity` but you can find some details on how to proceed in the
review by Penone et al. ([2014](#ref-Penone_Imputation_2014)).

## Functions summary table

| Function Name                                                                                     | Index Name     | Parallelizable[¹](#fn1) | Memoizable[²](#fn2) |
|:--------------------------------------------------------------------------------------------------|:---------------|:-----------------------:|:-------------------:|
| [`fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)                     | FRic           |           ✅            |         ✅          |
| [`fd_fric_intersect()`](https://funecology.github.io/fundiversity/reference/fd_fric_intersect.md) | FRic_intersect |           ✅            |         ✅          |
| [`fd_fdiv()`](https://funecology.github.io/fundiversity/reference/fd_fdiv.md)                     | FDiv           |           ✅            |         ✅          |
| [`fd_feve()`](https://funecology.github.io/fundiversity/reference/fd_feve.md)                     | FEve           |           ✅            |         ❌          |
| [`fd_fdis()`](https://funecology.github.io/fundiversity/reference/fd_fdis.md)                     | FDis           |           ❌            |         ❌          |
| [`fd_raoq()`](https://funecology.github.io/fundiversity/reference/fd_raoq.md)                     | Rao’s Q        |           ❌            |         ❌          |

## References

Gower, John C. 1971. “A General Coefficient of Similarity and Some of
Its Properties.” *Biometrics*, 857–71.

Leps, Jan, Francesco Bello, Sandra Lavorel, and Sandra Berman. 2006.
“Quantifying and Interpreting Functional Diversity of Natural
Communities: Practical Considerations Matter.” *Preslia* 78 (November):
481–501.

Maire, Eva, Gaël Grenouillet, Sébastien Brosse, and Sébastien Villéger.
2015. “How Many Dimensions Are Needed to Accurately Assess Functional
Diversity? A Pragmatic Approach for Assessing the Quality of Functional
Spaces.” *Global Ecology and Biogeography* 24 (6): 728–40.
<https://doi.org/10.1111/geb.12299>.

Nowak, Larissa, W. Daniel Kissling, Irene M. A. Bender, D. Matthias
Dehling, Till Töpfer, Katrin Böhning-Gaese, and Matthias Schleuning.
2019a. “Data from: Projecting Consequences of Global Warming for the
Functional Diversity of Fleshy-Fruited Plants and Frugivorous Birds
Along a Tropical Elevational Gradient.” *Data Dryad Digital Repository*.
<https://doi.org/10.5061/DRYAD.C0N737B>.

———. 2019b. “Projecting Consequences of Global Warming for the
Functional Diversity of Fleshy-Fruited Plants and Frugivorous Birds
Along a Tropical Elevational Gradient.” Edited by Kenneth Feeley.
*Diversity and Distributions* 25 (9): 1362–74.
<https://doi.org/10.1111/ddi.12946>.

Pavoine, Sandrine, Jeanne Vallet, Anne-Béatrice Dufour, Sophie Gachet,
and Hervé Daniel. 2009. “On the Challenge of Treating Various Types of
Variables: Application for Improving the Measurement of Functional
Diversity.” *Oikos* 118 (3): 391–402.
<https://doi.org/10.1111/j.1600-0706.2008.16668.x>.

Penone, Caterina, Ana D. Davidson, Kevin T. Shoemaker, Moreno Di Marco,
Carlo Rondinini, Thomas M. Brooks, Bruce E. Young, Catherine H. Graham,
and Gabriel C. Costa. 2014. “Imputation of Missing Data in Life-History
Trait Datasets: Which Approach Performs the Best?” *Methods in Ecology
and Evolution* 5 (9): 961–70. <https://doi.org/10.1111/2041-210X.12232>.

Podani, János. 1999. “Extending Gower’s General Coefficient of
Similarity to Ordinal Characters.” *Taxon*, 331–40.

Rao, C. Radhakrishna. 1982. “Diversity and Dissimilarity Coefficients: A
Unified Approach.” *Theoretical Population Biology* 21 (1): 24–43.
<https://doi.org/10.1016/0040-5809(82)90004-1>.

Villéger, Sébastien, Norman W. H. Mason, and David Mouillot. 2008. “New
Multidimensional Functional Diversity Indices for a Multifaceted
Framework in Functional Ecology.” *Ecology* 89 (8): 2290–2301.
<https://doi.org/10.1890/07-1206.1>.

------------------------------------------------------------------------

1.  parallelization through the `future` backend please refer to the
    [parallelization
    vignette](https://funecology.github.io/fundiversity/articles/fundiversity_1-parallel.html)
    for details.

2.  memoization means that the results of the functions calls are cached
    and not recomputed when recalled, to toggle it off see the
    [`fundiversity::fd_fric()`](https://funecology.github.io/fundiversity/reference/fd_fric.md)
    [Details
    section](https://funecology.github.io/fundiversity/reference/fd_fric.html#details).
