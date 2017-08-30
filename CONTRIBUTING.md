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
  - Install packages pre-requisites
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
