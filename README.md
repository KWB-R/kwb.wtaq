<img src="kwb_wtaq.png" alt="kwb.wtaq" />
 
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/7pkw4r01xttq2h6h/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-wtaq/branch/master)
[![Build Status](https://travis-ci.org/KWB-R/kwb.wtaq.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.wtaq)
[![codecov](https://codecov.io/github/KWB-R/kwb.wtaq/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.wtaq)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kwb.wtaq)](http://cran.r-project.org/package=kwb.wtaq
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.wtaq)](https://kwb-r.r-universe.dev/)
[![Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/kwb-r/kwb.wtaq/binder?urlpath=rstudio)

**Cite as:** [![DOI](https://zenodo.org/badge/23293/KWB-R/kwb.wtaq.svg)](https://zenodo.org/badge/latestdoi/23293/KWB-R/kwb.wtaq)

**Launch tutorial in cloud RStudio session** (and install kwb.wtaq from master 
branch): [![Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/kwb-r/kwb.wtaq/binder?urlpath=rstudio)

The R package **kwb.wtaq** provides a programmatic interface to the well drawdown model [WTAQ version 2.1](https://water.usgs.gov/ogw/wtaq/) (developed by the U.S. Geological Survey). The WTAQ 2.1 model 
engine is included in the package, but also available for download as [self-extracting win-zip file from the USGS](https://water.usgs.gov/ogw/wtaq/WTAQ_2.1.exe]) . 

## 1. Install from GitHub 

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.wtaq' from GitHub

remotes::install_github(repo = "KWB-R/kwb.wtaq", dependencies = TRUE)
```

## 2. Documentation

Release: [https://kwb-r.github.io/kwb.wtaq](https://kwb-r.github.io/kwb.wtaq)

Development: [https://kwb-r.github.io/kwb.wtaq/dev](https://kwb-r.github.io/kwb.wtaq/dev)

## 3. Known issues

Compilation of the WTAQ-2.1 source code (/src) throws the following **messages**:

- *"Error: Arithmetic overflow converting INTEGER(16) to INTEGER(4) at (1)"*
  (**fixed** in /src/Makevars with PKG_FFLAGS = -fno-range-check)
  
- *"Warning: Named COMMON block 'par6' at (1) shall be of the same size"*
  (**unfixed** *help in solving this is highly appreciated!*)


However, at least these issues do not cause calculation problems for the test carried out under windows:

```r
### Load/install required packages
if(!require("testthat")) { install.packages("testthat") }
library(kwb.wtaq)
library(testthat)

### Download test file from Github
test_url <- "https://raw.githubusercontent.com/KWB-R/kwb.wtaq/master/tests/testthat/test_windowsCompilation.R"
test_path <- file.path(tempdir(),"test_windowsCompilation.R")
download.file(test_url,destfile = test_path)

### Run test (only working on windows!)
testthat::test_file(test_path)

```
