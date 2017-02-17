# Welcome to healthcare.ai

This package will get you started with healthcare machine learning in R.

## What can you do with it?

* Create and compare models based on your data.
* Save and deploy a model.
* Perform risk-adjusted comparisons.
* Do trend analysis following [Nelson rules](https://en.wikipedia.org/wiki/Nelson_rules).
* Improve sparse data via longitudinal imputation.

## How is it specific to healthcare?

* Longitudinal machine learning via mixed models
* Longitudinal imputation
* Risk-adjusted comparisons

## How to install on Windows

* If you haven't, install [R](https://cran.cnr.berkeley.edu) and [RStudio](https://www.rstudio.com/products/rstudio/download)
* Grab prerequisites via the console of RGui or (preferably) RStudio  
```
install.packages(c('caret','data.table','devtools','doParallel','e1071','grpreg','lme4','lubridate','pROC','R6','ranger','ROCR','RODBC'),repos = "https://cran.cnr.berkeley.edu/")
```
* Install healthcare.ai
```
library(devtools)
devtools::install_url('https://github.com/HealthCatalystSLC/healthcareai-r/archive/v0.1.10.zip')
```

## How to install on macOS

Note: If using macOS with healthcare.ai, you'll have to write up your own database connectivity (or just use csv files). We'd [love to hear](http://healthcare.ai/contact) which databases your connecting to, so we can provide native support!

* Open the Terminal
* Install [Xcode](https://en.wikipedia.org/wiki/Xcode) compilers via `xcode-select –install`
* Accept the Xcode license via `sudo xcodebuild -license`
* Install [Homebrew](https://brew.sh/) (the macOS package manager) with
* Install ODBC driver via `brew update && brew install unixODBC`
* Open R Studio
* In the console, install RODBC from source with `install.packages('RODBC',type = "source")`
* In the console, install other R healthcare.ai prerequisites via
```
install.packages(c('caret','data.table','devtools','doParallel','e1071','grpreg','lme4','lubridate','pROC','R6','ranger','ROCR'),repos = "https://cran.cnr.berkeley.edu/")
```
* Install healthcare.ai
```
library(devtools)
devtools::install_url('https://github.com/HealthCatalystSLC/healthcareai-r/archive/v0.1.10.zip')
```

## Misc tips

* Note: if you want the bleeding edge version, use this:
```
library(devtools)
devtools::install_github(repo='HealthCatalystSLC/healthcareai-r')
```
* Load the package you just installed and read the built-in docs
```
library(healthcareai)
?healthcareai
```
* If you need assistance, check out our [Google Group](https://groups.google.com/forum/#!forum/healthcareai-users)!

## How to help

Check out our github [repo](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/README.md#contributing).
