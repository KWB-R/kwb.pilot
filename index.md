[![R-CMD-check](https://github.com/KWB-R/kwb.pilot/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.pilot/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.pilot/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.pilot/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.pilot/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.pilot)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) 
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.pilot)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.pilot)](https://kwb-r.r-universe.dev/)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.4564544.svg)](https://doi.org/10.5281/zenodo.4564544)

Importing, Aggregating and Visualising Data From KWB Pilot Plants. 

The R package [kwb.pilot](https://github.com/kwb-r/kwb.pilot) builds up on the 
R package [aquanes.report](https://github.com/kwb-r/aquanes.report) with the 
goal to generalise the functionality, for making it easy to use it in other 
KWB pilot-plants (e.g. in project SULEMAN).



# Installation

If you are interested in the capabilities of the R reporting tool, just make 
sure your computer satisfies the [software requirements](articles/A1_installation.html#pre-required-software) 
and subsequently execute the following lines of code below in R/RStudio. 

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.pilot in R
install.packages('kwb.pilot')


```


## Usage

In addition to the original [aquanes.report](https://github.com/kwb-r/aquanes.report), 
there are now three additional workflows (and many more functions), for the data 
import and aggregation of KWB pilot plants which are used in the following projects: 

- [MBR 4.0](articles/mbr40.html)

- [SULEMAN](articles/suleman.html)

- [ULTIMATE](articles/ultimate.html)

