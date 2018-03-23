# healthcareai

[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/0xrpe233o9a16l4l/branch/master?svg=true)](https://ci.appveyor.com/project/CatalystAdmin/healthcareai-r/) 
[![Travis-CI Build Status](https://travis-ci.org/HealthCatalyst/healthcareai-r.svg?branch=master)](https://travis-ci.org/HealthCatalyst/healthcareai-r) 
[![codecov badge](https://codecov.io/gh/HealthCatalyst/healthcareai-r/branch/master/graph/badge.svg)](https://codecov.io/gh/HealthCatalyst/healthcareai-r) 

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/healthcareai)](https://cran.r-project.org/package=healthcareai)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.999334.svg)](https://doi.org/10.5281/zenodo.999334)

Machine learning as easy as...

```r
library(healthcareai)
models <- machine_learn(d = pima_diabetes, outcome = diabetes)
predict(models)
```

**Please note that the package is currently in transition** from an R6 architecture to a tidy, S3 architecture. The code above is the new version, which is in beta and can be acquired via `devtools::install_github("HealthCatalyst/healthcareai-r")`. Beta testing of the new version, pre-CRAN, is very welcome; please file issues for any bugs, unclear docs, or feature suggestions here. `install.packages("healthcareai")` will get you the old version from CRAN, which will remain available via `devtools::install_github("HealthCatalyst/healthcareai-r@v1.2.3")`.


The aim of `healthcareai` is to make machine learning on healthcare data easy. The package does that by providing functions to:

- Easily develop a high performance machine-learning model tuned to your data. 
- Make predictions based on that model and push them to a database or flat file.
-  Provide tools related to data cleaning, manipulation, imputation, and visualization.

## Getting started

- If you haven't, install [R](https://CRAN.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download)
    + Note: if you're setting up R on an ETL server, don't download RStudio--simply open up RGui or work in the terminal.
- Open RStudio and install the package by entering this in the console: `install.packages('healthcareai')`

#### Website examples

See our [docs website](http://healthcareai-r.readthedocs.io).

#### Join the community

Read the [blog](http://healthcare.ai/blog/), check out our machine learning [broadcasts](https://www.youtube.com/channel/UCGZUobs_x712KbcL6RSzfnQ), and join the [Slack group](https://healthcare-ai.slack.com/).

## For issues

- If you're having trouble getting code to work, the [healthcare-ai Slack community](https://healthcare-ai.slack.com/) is a great place to ask question. If you think something may be broken, you can file a [Github issue](https://github.com/HealthCatalyst/healthcareai-r/issues) or submit a question on [Stack Overflow](http://stackoverflow.com/) using the healthcare-ai tag. Please provide:
  + Details on your environment. The output `devtools::session_info()` is great for this.
  + A concise discussion of what are you trying to accomplish.
  + A [reproducible example](https://github.com/tidyverse/reprex) of the error.

## Contributing

You want to help? Woohoo! We welcome that and are willing to help newbies get started.

First, see [here](CONTRIBUTING.md) for instructions on setting up your development environment and how to contribute. You'll want to install the development version of the package via 'devtools::install_github(repo='HealthCatalyst/healthcareai-r')`.

## Troubleshooting

Does `install.packages('healthcareai')` or `library(healthcareai)` fail? If you don't have admin rights on the machine you are working on, you may need to set a custom location for your R libraries. Here's how to do that:

1. *Create a folder to hold your R packages.* You'll generally have write access to your `Documents` folder, so you might create a new directory: `C:\Users\your.name\Documents\R\R_library`. Shift-right click on that folder and copy its path.
2. *Define a system variable with that folder location.* Open the Control Panel and click through User Accounts -> User Accounts -> Change my environment variables, and add a variable called `R_LIBS_USER`, and paste the folder path (`C:\Users\your.name\Documents\R\R_library`) into the value field. Make sure the path is not surrounded by `"`s.
3. *Tell R to use that location.* Restart R Studio, run `install.packages('healthcareai')`, and if asked whether you want to use a custom library location choose yes, which may be sufficient. If not, click into the Console in R Studio, type `.libPaths()`, paste the path to your new library folder inside the `()`, and change the `\`s to `/`. You should end up with a line that looks like: `.libPaths("C:/Users/your.name/Documents/R/R_library")`. Press enter to run that.
4. *Try again.* Run `install.packages('healthcareai')` and `library(healthcareai)` again and all should be well! 

If that doesn't work, please contact us through Github or at the website linked above.

#### Looking for an older version of the package? 

Version 1 of healthcare-ai has been retired, but if you want to to continue to use its functionality, you can still install it from github: `devtools::install_github("HealthCatalyst/healthcareai-r@v1.2.4")`.
