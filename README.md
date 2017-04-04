# healthcareai

[![Build status](https://ci.appveyor.com/api/projects/status/0xrpe233o9a16l4l/branch/master?svg=true)](https://ci.appveyor.com/project/CatalystAdmin/healthcareai-r/) 
[![Travis-CI Build Status](https://travis-ci.org/HealthCatalystSLC/healthcareai-r.svg?branch=master)](https://travis-ci.org/HealthCatalystSLC/healthcareai-r)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/HealthCatalystSLC/healthcareai-r/blob/master/LICENSE)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/healthcareai)](https://cran.r-project.org/package=healthcareai)


The aim of `healthcareai` is to make it easy to do machine learning with healthcare 
data. The package has two main goals:

-  Allow one to easily create models based on tabular data, and deploy a best
model that pushes predictions to SQL Server.

-  Provide tools related to data cleaning, manipulation, and imputation.

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

3) Create a throwaway file on the Desktop (or somewhere outside the repo), based on an example

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

   5. Under the build tab, run 'Check' and verify that only one roxygen warning arises
      - This warning is due to the [limitations](https://github.com/wch/R6/issues/3) of roxygen and R6 method documentation
      - This is the only warning/error/note that's allowed when merging to master
      
   6. Now that your changes are working, communicate that to Levi in the pull request, such that he knows to do the code review associated with the PR. Please *don't* do tons of work and *then* start a PR. Early is good.
