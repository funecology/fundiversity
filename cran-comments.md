## Test environments

* local Windows install, R 4.2.1
* macOS (on GitHub actions), R release
* ubuntu 20.04 (on GitHub actions), R oldrel, R release, R-devel
* Windows (on GitHub actions), R release
* win-builder (R release and devel)
* devtools::check_rhub()


## R CMD check results

In most cases,

0 errors | 0 warnings | 0 note


## Comments

One NOTE due to possibly invalid URLS: https://doi.org/10.1111/2041-210X.12232; 
https://doi.org/10.1111/2041-210X.13430; https://doi.org/10.1111/ddi.12946; 
https://doi.org/10.1111/geb.12299;
https://doi.org/10.1111/j.1600-0706.2008.16668.x; 
https://doi.org/10.1146/annurev-ecolsys-120213-091540;
https://doi.org/10.1890/07-1206.1; https://doi.org/10.21105/joss.01041

All of them resolve perfectly when tested on two different browsers.

Sometimes on (Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC),

* checking CRAN incoming feasibility ... NOTE

Possibly misspelled words in DESCRIPTION:
  preprint (20:4)

but the word is well written.
