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

- If you haven't, install [R](https://cran.cnr.berkeley.edu) version >= 3.2.3 and [RStudio](https://www.rstudio.com/products/rstudio/download)

Note: if you're setting up R on an ETL server, don't download RStudio--simply open up RGui

## Install the latest release on Windows

Open RStudio and work in the console
```
install.packages('healthcareai')
```

## How to install the latest version on macOS

Open RStudio and work in the console
```
install.packages('healthcareai')
```

## How to install latest version on Ubuntu (Linux)

* An Ubuntu 14.04 Droplet with at least 1 GB of RAM is required for the installation.
* Follow steps 1 and 2 [here](https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04) to install R
* Run `sudo apt-get install libiodbc2-dev`
* Run `sudo apt-get install unixodbc unixodbc-dev`
* After typing `R` run `install.packages('healthcareai')`

## Install the bleeding edge version (for folks providing contributions)

Open RStudio and work in the console 
```
library(devtools)
devtools::install_github(repo='HealthCatalyst/healthcareai-r')
```

## Tips on getting started

#### Built-in examples
Load the package you just installed and read the built-in docs
```
library(healthcareai)
?healthcareai
```

#### Website examples
See our [docs website](http://healthcareai-r.readthedocs.io)

#### Jupyter notebook examples
If you like Jupyter notebooks, check out [step 1](https://github.com/HealthCatalyst/documentation/blob/master/notebooks/Example1.ipynb) and [step 2](https://github.com/HealthCatalyst/documentation/blob/master/notebooks/Example2.ipynb) in model building with healthcareai.

## Join the community
Read the blog and join the slack channel at [healthcare.ai](https://healthcare.ai)

## What's new?
The CRAN 1.0.0 release features:
- Added: 
  - Kmeans clustering
  - XGBoost multiclass support
  - findingVariation family of functions
- Changed: 
  - Develop step trains and saves models
  - Deploy no longer trains. Loads and predicts on all rows.
  - SQL uses a DBI back end
- Removed:
  - `testWindowCol` is no longer a param.
  - SQL reading/writing is outside model deployment.

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

Check out our github [repo](https://github.com/HealthCatalyst/healthcareai-r/blob/master/README.md#contributing).
