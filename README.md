# healthcareai

[![Build status](https://ci.appveyor.com/api/projects/status/0xrpe233o9a16l4l/branch/master?svg=true)](https://ci.appveyor.com/project/CatalystAdmin/healthcareai-r/) 
[![Travis-CI Build Status](https://travis-ci.org/HealthCatalyst/healthcareai-r.svg?branch=master)](https://travis-ci.org/HealthCatalyst/healthcareai-r)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/LICENSE)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/healthcareai)](https://cran.r-project.org/package=healthcareai)


The aim of `healthcareai` is to make machine learning easy on healthcare data. The package has two main goals:

-  Allow one to easily develop and compare models based on tabular data, and deploy a best model that pushes predictions to either databases or flat files.

-  Provide tools related to data cleaning, manipulation, and imputation.

## For those starting out

- If you haven't, install [R](https://CRAN.r-project.org/) version >= 3.2.3 and [RStudio](https://www.rstudio.com/products/rstudio/download)

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

## Contributing

You want to help? Woohoo! We welcome that and are willing to help newbies get started.

First, see [here](CONTRIBUTING.md) for instructions on setting up your development environment

After that's done, *here's the contribution workflow:*

1) Identify an issue that suits your skill level [here](https://github.com/HealthCatalystSLC/healthcareai-r/issues)
   - Only look for issues in the Backlog category
   - If you're new to open source, please look for issues with the `bug low`, `help wanted`, or `docs` tags
   - Please reach out with questions on details and where to start

2) Create a topic branch to work in, as described [here](CONTRIBUTING.md#create-a-topic-branch-that-you-can-work-in)

3) To test the new functionality you've created, use a new throw-away file on the Desktop (or somewhere outside the repo), perhaps based on a package example

4) As you make changes
   - Document any new functions, methods, etc via [roxygen2](http://r-pkgs.had.co.nz/man.html)
   - Be sure to build often
     - Note: building checks for errors and allows you to use the latest code
   - Make small commits after getting a small piece working
   - Push often so your changes are backed up. See [here](https://gist.github.com/blackfalcon/8428401#push-your-branch) for more
   
5) Early on, create a [pull request](https://yangsu.github.io/pull-request-tutorial/) such that Levi and team can discuss the changes that you're making. Conversation is good.
   
6) When you're done with the issue you chose, do the following
   
   1. Merge the master branch into your topic branch (so that you have the latest changes from master)
   
   ```
   git checkout LeviBugFix
   git fetch
   git merge --no-ff origin/master
   ```
     - This opens a text editor called [vim](https://github.com/yuanqing/vim-basics/blob/master/README.md), where you type `i`, type your commit message, and [then save](http://stackoverflow.com/a/6098842/5636012)
   
   2. Build and fix any errors

   3. Run tests via `devtools::tests()` or CTRL+SHIFT+D and fix any errors
   
   4. Run the roxygen2 examples via `devtools::run_examples()` and fix any errors

   5. Under the build tab, run 'Check' and verify that no errors/warnings/notes arise
      
   6. Now that your changes are working, communicate that to Levi and team in the pull request, such that he knows to do the code review associated with the PR. Please *don't* do tons of work and *then* start a PR. Early is good.
