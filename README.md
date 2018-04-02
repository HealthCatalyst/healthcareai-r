
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthcareai <img src="man/figures/logo.png" align="right" />

[![Appveyor Build
Status](https://ci.appveyor.com/api/projects/status/0xrpe233o9a16l4l/branch/master?svg=true)](https://ci.appveyor.com/project/CatalystAdmin/healthcareai-r/)
[![Travis-CI Build
Status](https://travis-ci.org/HealthCatalyst/healthcareai-r.svg?branch=master)](https://travis-ci.org/HealthCatalyst/healthcareai-r)
[![codecov
badge](https://codecov.io/gh/HealthCatalyst/healthcareai-r/branch/master/graph/badge.svg)](https://codecov.io/gh/HealthCatalyst/healthcareai-r)

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/healthcareai)](https://cran.r-project.org/package=healthcareai)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.999334.svg)](https://doi.org/10.5281/zenodo.999334)

## Overview

The aim of `healthcareai` is to make machine learning in healthcare as
easy as possible. It does that by providing functions to:

  - Develop customized, reliable, high-performance machine learning
    models with minimal code
  - Easily make and evaluate predictions and push them to a database or
    flat file
  - Provide tools related to data cleaning, manipulation, imputation,
    and visualization

**Please note that the package is currently in transition** from an R6
architecture to a tidy, S3 architecture. The code below is the new
version, which is in beta and can be acquired via
`remotes::install_github("HealthCatalyst/healthcareai-r")`. Beta testing
of the new version, pre-CRAN, is very welcome; please file issues for
any bugs, unclear docs, or feature suggestions here.
`install.packages("healthcareai")` will get you the old version from
CRAN, which will remain available via
`remotes::install_github("HealthCatalyst/healthcareai-r@v1.2.4")`.

## Example

`healthcareai` can take you from messy data to an optimized model in one
line of code:

``` r
models <- machine_learn(pima_diabetes, patient_id, outcome = diabetes)
```

Visually inspect the perforance of models:

``` r
plot(models)
```

![](readme_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Make predictions:

``` r
predictions <- predict(models)
```

Examine predictive performance:

``` r
plot(predictions)
```

![](readme_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
