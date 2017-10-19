# Development guidelines for the S3 Refactor

## Organization

- All work on the refactor should be done in branches off of, and PRs should be to, `refactorS3`.

- In general, each function should get its own file with the filename matching the function name. 
    * Verb function names are good, e.g. `find_correlations`, `predict`
    * Exceptions to one-function-per-file:
        + Class definition files should be called `class.R` and contain `class()` and `as.class()` constructor functions and a `is.class` test function. 
        + Short, non-exported utility functions can go in the `utilities.R` file. 
            * Use the `@noRd` tag in the roxygen documentation of these functions and be sure not to `@export` them.
        + Non-exported functions that will likely only be called by one exported function can go below the calling function in its file. This is useful to keep individual functions from getting too long. 

- When you write a new function:
    * Place the old function in the `R/deprecated.R` file.
    * Use [`.Deprecated` or `.Defunct`](https://stackoverflow.com/a/10145627/2565816) to point the user to the new function.
    * Delete the old function's tests and be sure your new function is tested.

## Documentation

- Document using `roxygen2`. Be sure to read the [documentation section](http://style.tidyverse.org/code-documentation.html) of the style guide. 

- Keep examples succinct. In general, one example showing the minimal use of the function (with defaults) and one example showing a customized use should be adequate. Longer examples can be vignettes.

## Testing

- We use `testthat` for testing. Write tests before you write function code. Read the [testing chapter in Hadley's R packages](http://r-pkgs.had.co.nz/tests.html) about how to write good tests.

- At the end of any test file that creates data or log files, cleanup the files with `file.remove`.

## Code Style

We abide by the [tidyverse style guide](http://style.tidyverse.org/). Additional style details below.

- We use `lintr` to check code style. You can check your style with `lint_package()`. It will also be checked on `devtools::test()` and on travis.
    * Should you need to violate `lintr` rules, put `# nolint` at the end of the violating line.
    * There is a Sublime integration for lintr, [here](https://github.com/jimhester/lintr#sublime-text-3).

- We use `strict` to enforce betteR practices, but RStudio [currently breaks](https://github.com/hadley/strict/issues/5) if we try to load the package automatically at startup, so for now, run `library(strict)` whenever you're developing.

#### Additional `dplyr` Details

- Piped lines of code should start with a line that is just the name of the data frame. Assignment should go on its own line. E.g.

```r
df <-
  df %>%
    filter(!is.na(var))
```

- To extract a vector from a data frame at the end of a piped sequence, use `dplyr::pull(var)` rather than `.[[var]]` or `.$var`. 

- For programming with dplyr, e.g. `filter(df, columnFromUser > 0)`, run `dplyr`'s `vignette("programming")`

## Package Management

We're staying current with other R packages, so it's a good idea to develop with updated versions of R and packages. Update R from [CRAN](https://cran.r-project.org/) and then update packages with `update.packages(ask = FALSE)`. Updating packages will take a few minutes if you haven't done it recently.

## TBD

[`vtreat`](https://github.com/WinVector/vtreat/) implements impact coding and deals with a bunch of common problems we often run into. We should use it. It would be great to bake it into the recipes pipeline.


---

*The following is the old CONTRIBUTING doc, which should be cleaned-up and restored with the above integrated post refactor*

# Setting up your development environment (to enable contributions)

## Set up R, RStudio, and healthcare.ai

1. [Download](http://cran.us.r-project.org/) latest R and install
2. [Download](https://www.rstudio.com/products/rstudio/download3/) RStudio and install
3. Follow the healthcare.ai [install instructions](https://github.com/HealthCatalystSLC/HCRTools/blob/master/README.md)

## Set up Git 

### Windows

- Download and install [Git for Windows](https://git-scm.com/download/win)
  - Choose "64-bit Git for Windows Setup"
  - On the Select Components screen, accept the defaults
  - After selecting install location, choose "Use Git from the Windows Command Prompt"
  - Checkout using Windows-style
  - Choose MinTTy
  - Restart RStudio
- Open RStudio
  - Open Tools --> Global Options
  - Look for git.exe in the bin folder in C:\Program Files\Git\bin

### macOS

- Open the Mac Terminal
- Install [Xcode](https://en.wikipedia.org/wiki/Xcode) compilers via `xcode-select --install`
- Accept the Xcode license via `sudo xcodebuild -license`

### Linux

- [Git Install](https://git-scm.com/download/linux)

## Clone healthcareai-r repo

- Create a data science folder on your hard drive (if you don't have one)
- Fork this repo (look for the link in the top right corner [here](https://github.com/HealthCatalystSLC/healthcareai-r)).
- Click the green 'Clone or download' button and copy git link. You'll want to use the HTML address unless you have an SSH key for git already.
- Open RStudio
  - Install package prerequisites
  ```install.packages(c('caret','data.table','DBI','doParallel','e1071','grpreg','lme4','odbc','pROC','R6','ranger','ROCR','RSQLite','xgboost'),repos = "https://cran.cnr.berkeley.edu/")```
  - Look for Project dropdown (top right corner)
  - Click New Project
  - Click Version Control
  - Click Git
  - Paste git link into 'Repository URL' and create project as subdirectory of your data science folder
- Note that a git tab now appears in RStudio

## Create a topic branch that you can work in

1. In RStudio -> Tools (at top) -> Shell
2. Type `git checkout -b nameofbranch`
   - This creates the your local branch for work
   - Note this branch should have your name in it
3. Type `git push origin nameofbranch`
   - This pushes the branch to github (and now it's backed-up)

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

### RStudio Packages

- In RStudio, install these packages via Tools -> Install Packages
  - devtools
  - roxygen2
  - testthat
  - evaluate
 
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

## RStudio :: Configure git

1. Open RStudio -> Tools (at top) -> Shell
2. Set up your email and user name (for [proper attribution](https://help.github.com/articles/setting-your-username-in-git/))
   - `git config user.name "Billy Everyteen`
   - `git config --global user.email "your_email@example.com"`
3. Configure line endings via `git config core.autocrlf true`
4. Make git case-sensitive via `git config core.ignorecase false`
5. Improve merge conflict resolution via `git config --global merge.conflictstyle diff3`
6. Set up SSH so you can push without typing password (optional, but nice)
   - See [here](https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/), [here](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/), and then [here](https://help.github.com/enterprise/11.10.340/user/articles/changing-a-remote-s-url/)
   
## Getting started with contributions

- When your dev environment is set up, see [here](README.md#contributing) for the contribution workflow.
- We try to adhere to [Hadley's R styleguide](http://adv-r.had.co.nz/Style.html) when writing code together. Overall:
  - File names are like this: `cool-helpful-file.R`
  - Function names are like this: `coolHelpfulFunction`
  - Variable names are like this: `coolHelpfulVariable`
- We try not introduce new package dependencies if we can avoid them.
- Use :: when calling a function to show what namespace it comes from. For example: `healthcareai::findTrends()`. This makes debugging easier and prevents overloading.

## RStudio Tips

- How to load a project
    + The menu on the upper right of the screen contains the projects.
- How to load (instead of build) to use your new function
    + **CTRL-SHIFT-L**
- How to document functions via roxygen
    + **CTRL-SHIFT-D**
- How to build
    + **CTRL-SHIFT-B**
- All Keyboard Shortcuts
    + **ALT-SHIFT-K**
- How to run unit tests
    - **CTRL-SHIFT-T**
- Navigate to file/function
    + **CTRL-.**
    - `devtools::test(filename)` runs a single test file
- How to run examples
    `devtools::run_examples()`
- How (and why) to Check
    + **CTRL-SHIFT-E**
    + runs unit test
    + runs examples
    + runs build and reload
- The [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/) are chock full of helpful hints.
