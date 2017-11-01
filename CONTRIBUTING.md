# Setting up your development environment (to enable contributions)

## Set up R, RStudio, and healthcare.ai

1. [Download](http://cran.us.r-project.org/) latest R and install
2. [Download](https://www.rstudio.com/products/rstudio/download3/) RStudio and install
3. [Download](https://desktop.github.com/) Github desktop and install


## Fork healthcareai-r repo

- Fork the repo (look for the link in the top right corner [here](https://github.com/HealthCatalyst/healthcareai-r)). This copies the repo from our organization onto your personal account, so you can modify it.
- Click the green 'Clone or download' button and click, "Open in Desktop." This will take you to github desktop, where you can select a folder for your local copy of the repo.
- Open RStudio
  - Install package prerequisites
  ```install.packages(c('caret','data.table','DBI','doParallel','e1071','grpreg','lme4','odbc','pROC','R6','ranger','ROCR','RSQLite','tidyverse','xgboost'),repos = "https://cran.cnr.berkeley.edu/")```
  - Install development requirments
  `install.packages(c('devtools','evaluate','roxygen2','testthat')`
  - Look for Project dropdown (top right corner)
  - Click `New Project`
  - Click `From Existing Directory`
  - Navigate to the folder you selected above for your local copy of the repository.
- Note that a git tab now appears in RStudio

## Create a topic branch that you can work in

- In github desktop, go to the `Branch` menu and select, `New Branch`.
- Title your new branch with your name, the issue number, and the purpose, like, `mike_678_bugfix`.

## RStudio Configuration

- In RStudio, if you don't see healthcareai-r in the top right corner
  - Click the project tab in the top-right -> Open Project
  - Select the healthcareai-r.Rproj file (that's at the top-level of the repo you downloaded)
- Click on Tools (at top menu of RStudio) -> Project Options -> Build Tools
  - Check 'Use devtools package functions if available
  - Check 'Generate documentation with Roxygen' If you don't see this option, you must install Roxygen2 by entering the following into the command line:
  install.packages("roxygen2")
  - Click on 'Configure' Button -> Check all boxes
  - Click OK twice
- Click again on Tools -> Global Options
  - Under Code -> Editing, check 'Always save R scripts before sourcing'
  - Under Code -> Display, check check 'Show margin' and set to 80 characters
  - Under Code -> Diagnostics, check all boxes

## Set up environment for R develeopment

### Windows

- [Download](https://cran.r-project.org/bin/windows/Rtools/) and install RTools
  - Note: this installs a C++ compiler (called [g++](https://gcc.gnu.org/onlinedocs/gcc-3.3.6/gcc/G_002b_002b-and-GCC.html))
  - Can accept defaults on `Select Destination Location` and `Select Components` screens
  - On `Select Additional Tasks` step, check the PATH box
  - If you ever see issues related to Rtools, see [here](https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows)

### macOS

- install [Xcode](https://developer.apple.com/xcode/) if you haven't already
 
## Verify you can build the healthcareai package

1. In RStudio, if you don't see healthcareai-r in the top right corner
   - Click the project tab in the top right -> Open Project
2. Click on the build tab in the top right corner
3. Click Build and verify that you can build successfully (without errors)
   - Note that you may see warnings about versioning, which is fine
4. Run tests via `devtools::test()` or CTRL+SHIFT+D or Build dropdown -> Test Package
   - Verify that these pass without seeing errors
5. Run the roxygen2 examples via `devtools::run_examples()`. Verify that these finish without errors
   - Verify that these pass without seeing errors
6. Under the build tab, run 'Check'
   - Verify that no errors/warnings arise
   
# Contribution Workflow

1) Identify an issue that suits your skill level [here](https://github.com/HealthCatalyst/healthcareai-r/issues)
   - Only look for issues in the Backlog category
   - If you're new to open source, please look for issues with the `bug low`, `help wanted`, or `docs` tags
   - Please reach out with questions on details and where to start

2) Create a topic branch to work in, as described [here](CONTRIBUTING.md#create-a-topic-branch-that-you-can-work-in)

3) To test the new functionality you've created, use a new throw-away file on the Desktop (or somewhere outside the repo), perhaps based on a package example

4) As you make changes in the code:
   - Use `Ctrl+Shift+L` to load your changes as you test them iteratively.
   - Make small commits after getting a small piece working.
   - Push often so your changes are backed up to the remote. See [here](https://gist.github.com/blackfalcon/8428401#push-your-branch) for more
   - Document any new functions, methods, etc via [roxygen2](http://r-pkgs.had.co.nz/man.html)
   - Merge the master branch into your topic branch (so that you have the latest changes from master) using the `Sync` button in github desktop.
   
5) Create a [pull request](https://yangsu.github.io/pull-request-tutorial/)
- Early is best! That way, our team can discuss the changes that you're making. Conversation is good.
- Using the `Build` tab in Rstudio:
  - Build the package. This checks for errors and indexes all the functions.
  - Build the documentation.
  - Check the package and look for errors, warnings, and notes.
  - If you run into problems, check the [RStudio Tips](CONTRIBUTING.md#RStudio-Tips) section below.
  - If your changes are ready, comment (with an `@team_member`) in the pull request to let us know is ready. Again, please *don't* do tons of work and *then* start a PR. Early is good.
   
6) Address your review changes
  - Fix whatever we comment on.
  - As you make fixes, new pushes will go to the same pull request automatically.
  - When everyone is happy with the code, we'll merge it into the package. Nice job!

# RStudio Tips

- Navigate to file/function
    + **CTRL-.**
- All Keyboard Shortcuts
    + **ALT-SHIFT-K**
- How to load a project
    + The menu on the upper right of Rstudio contains the projects.
- How to load (instead of build) to use your new function
    + **CTRL-SHIFT-L**
- How to document functions via roxygen
    + **CTRL-SHIFT-D**
- How to build
    + **CTRL-SHIFT-B**
- How to run unit tests
    - **CTRL-SHIFT-T**
    - `devtools::test(filter = "filename")` runs a single test file
- How to run examples
    `devtools::run_examples()`
- How (and why) to Check
    + **CTRL-SHIFT-E**
    + runs build and reload
    + runs unit test
    + runs examples
- The [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/) are chock full of helpful hints.
