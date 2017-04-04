# Welcome to healthcare.ai

This package will get you started with healthcare machine learning in R.

Find our code at our [Github Repo ![Our Github repo](img/GitHub-Mark-120px-plus.png)](https://github.com/HealthCatalystSLC/healthcareai-r)

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

## For those starting out

- If you haven't, install [R](https://cran.cnr.berkeley.edu) and [RStudio](https://www.rstudio.com/products/rstudio/download)

Note: if you're setting up R on an ETL server, don't download RStudio--simply open up RGui

## Install the latest release on Windows

Open RStudio and work in the console

```
install.packages('healthcareai')
install.packages('assertthat')
```

## How to install the latest version on macOS

Note: If using macOS with healthcare.ai, you'll have to use csv files. We're working on adding MySQL connections. We'd [love to hear](http://healthcare.ai/contact) which other databases your connecting to, so we can provide native support!

* Open the Mac Terminal
* Install [Xcode](https://en.wikipedia.org/wiki/Xcode) compilers via `xcode-select –install`
* Accept the Xcode license via `sudo xcodebuild -license`
* Install [Homebrew](https://brew.sh/) (the macOS package manager) with
* Install ODBC driver via `brew update && brew install unixODBC`
* Open R Studio
* In the console, install RODBC from source with `install.packages('RODBC',type = "source")`
* In the console, install other R healthcare.ai prerequisites via
```
install.packages(c('caret','data.table','doParallel','e1071','grpreg','lme4','lubridate','pROC','R6','ranger','ROCR'),repos = "https://cran.cnr.berkeley.edu/")
```
* Install healthcare.ai
```
install.packages('healthcareai')
install.packages('assertthat')
```

## How to install latest version on Ubuntu (Linux)

* Follow steps 1 and 2 [here](https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04) to install R
* Run `sudo apt-get install libiodbc2-dev`
* Run `sudo apt-get install r-cran-rodbc`
* After typing `R` run `install.packages('healthcareai')` and `install.packages('assertthat')`

## Install the bleeding edge version (for folks providing contributions)

* Grab prerequisites via the console of RGui or (preferably) RStudio  
```
install.packages(c('caret','data.table','devtools','doParallel','e1071','grpreg','lme4','lubridate','pROC','R6','ranger','ROCR','RODBC'),repos = "https://cran.cnr.berkeley.edu/")

library(devtools)
devtools::install_github(repo='HealthCatalystSLC/healthcareai-r')
```

## Misc tips

- Load the package you just installed and read the built-in docs
```
library(healthcareai)
?healthcareai
```
- If you like Jupyter notebooks, check out [step 1](https://github.com/HealthCatalystSLC/documentation/blob/master/notebooks/Example1.ipynb) and [step 2](https://github.com/HealthCatalystSLC/documentation/blob/master/notebooks/Example2.ipynb) in model building with healthcareai.

## What's new?

Since the CRAN 0.1.11 release, the following has been added to the bleeding edge version

- Output nightly predictions to a dataframe for use with MySQL, Oracle, etc. See the csv example at ?RandomForestDeployment
- Evaluate in-the-wild performance via AU_ROC and AU_PR scores. See more at ?generateAUC

## For issues

- Double check that the code follows the examples in the built-in docs
```R
library(healthcareai)
?healthcareai
```
  
- Make sure you've thoroughly read the descriptions found [here](http://healthcareai-r.readthedocs.io)

- If you're still seeing an error, file an issue on [Stack Overflow](http://stackoverflow.com/) using the healthcare-ai tag. Please provide
  - Details on your environment (OS, database type, R vs Py)
  - Goals (ie, what are you trying to accomplish)
  - Crystal clear steps for reproducing the error

## How to help

Check out our github [repo](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/README.md#contributing).
