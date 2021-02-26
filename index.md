[![R-CMD-check](https://github.com/KWB-R/kwb.pilot/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.pilot/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.pilot/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.pilot/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.pilot/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.pilot)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) 


Importing, Aggregating and Visualising Data From KWB Pilot Plants

# Demo of reporting tool for Haridwar site   

If you are interested in the capabilities of the R reporting tool, just make sure your computer satisfies the [software requirements](articles/A1_installation.html#pre-required-software) and subsequently execute the following lines of code below in R/RStudio. 

```r
if (!require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

remotes::install_github("kwb-r/aquanes.report", 
                         dependencies = TRUE)

aquanes.report::run_app()

```

This will launch the R reporting tool in your default webbrowser. The demo is based on real operational and analytical data for the AQUANES site Haridwar for the time period from 2017-03-24 until 2017-07-25. 
