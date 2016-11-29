# Setting up your dev environment (to enable contributions)

## Set up R, IDE, and healthcare.ai

1) [Download](http://cran.us.r-project.org/) latest R and install

2) [Download](https://www.rstudio.com/products/rstudio/download3/) RStudio and install

3) Follow the healthcare.ai [install instructions](https://github.com/HealthCatalystSLC/HCRTools/blob/master/README.md)

## Set up Git 

This is for Windows; for macOS see [here](https://developer.apple.com/xcode/); for linux see [here](https://git-scm.com/download/linux)

- Download and install [Git for Windows](https://git-scm.com/download/win)
  - Choose Use Git from Git Bash only
  - Checkout using Windows-style
  - Choose Mintty
- Restart RStudio
- Open RStudio
  - Open Tools --> Global Options
  - Look for git.exe in the bin folder in C:\Program Files\Git\bin

## Clone healthcareai-r repo

-	Create a data science folder (if you don't have one)
-	In github repo, click green 'Clone or download' and copy git link
-	Open RStudio
  - Look for Project dropdown (top right corner)
  - Click New Project
  -	Click Version Control
  - Click Git
  -	Copy git link into 'Repository URL' and create project as subdirectory of your data science folder
-	Note that a git tab now appears in RStudio

## Create a topic branch that you can work in
1) In RStudio, click Git tab -> Gear Icon -> Shell

2) Type `git checkout -b nameofbranch`
   - This creates the your local branch for work
   - Note this branch should have your name in it
   
3) Type `git push origin nameofbranch`
   - This pushes the branch to github (and now it's backed-up)

## RStudio Configuration

- In RStudio, if you don't see healthcareai-r in the top right corner
  - Click the project tab in the top-right -> Open Project
  - Select the healthcareai-r.Rproj file (that's at the top-level of the repo you downloaded)
- Click on Tools (at top menu of RStudio) -> Project Options -> Build Tools
  - Check 'Use devtools package functions if available
  - Check 'Generate documentation with Roxygen'
  - Click on 'Configure' Button -> Check all boxes
  - Click OK twice
- Click again on Tools -> Global Options
  - Under Editing, check 'Always save R scripts before sourcing'
  - Under Display, check check 'Show margin' and set to 80 characters
  - Under Diagnostics, check all boxes

## Set up environment for R dev

- If using Windows, [Download](https://cran.r-project.org/bin/windows/Rtools/) and install RTools
  - Note: this installs a C++ compiler (called [g++](https://gcc.gnu.org/onlinedocs/gcc-3.3.6/gcc/G_002b_002b-and-GCC.html))
  - Can accept defaults on `Select Destination Location` and `Select Components` screens
  - On `Select Additional Tasks` step, check the PATH box
  - If you ever see issues related to Rtools, see [here](https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows)
- If using macOS, install [Xcode](https://developer.apple.com/xcode/) if you haven't already
- In RStudio, install these packages via Tools -> Install Packages
  - devtools
  - roxygen2
  - testthat
  - evaluate
- Set up SQL Server, if you haven't already
  - If on Windows, [install](http://stackoverflow.com/a/11278818/5636012) both SQL Server Express and SSMS Express
  - Create tables (on localhost) to receive predictive output using the code below (use SSMS if on Windows):
  - Note that these will go in the SAM database, if using the Health Catalyst analytics environment
```SQL
CREATE TABLE [dbo].[HCRDeployClassificationBASE](
	[BindingID] [int] NULL,
	[BindingNM] [varchar](255) NULL,
	[LastLoadDTS] [datetime2](7) NULL,
	[PatientEncounterID] [decimal](38, 0) NULL,
	[PredictedProbNBR] [decimal](38, 2) NULL,
	[Factor1TXT] [varchar](255) NULL,
	[Factor2TXT] [varchar](255) NULL,
	[Factor3TXT] [varchar](255) NULL
)

CREATE TABLE [dbo].[HCRDeployRegressionBASE](
	[BindingID] [int] NULL,
	[BindingNM] [varchar](255) NULL,
	[LastLoadDTS] [datetime2](7) NULL,
	[PatientEncounterID] [decimal](38, 0) NULL,
	[PredictedValueNBR] [decimal](38, 2) NULL,
	[Factor1TXT] [varchar](255) NULL,
	[Factor2TXT] [varchar](255) NULL,
	[Factor3TXT] [varchar](255) NULL
)

CREATE TABLE [dbo].[HCRWriteData](
	[a] [float] NULL,
	[b] [float] NULL,
	[c] [varchar](255) NULL
)
```
  
## Verify you can build the healthcareai package

1) In RStudio, if you don't see healthcareai-r in the top right corner
   - Click the project tab in the top right -> Open Project
   
2) Click on the build tab in the top right corner
   
3) Click Build and verify that you can build successfully (without errors)
   - Note that you may see warnings about versioning, which is fine
   
4) Run tests via `devtools::tests()` or CTRL+SHIFT+D or Build dropdown -> Test Package
   - Verify that these pass without seeing errors
   
5) Run the roxygen2 examples via `devtools::run_examples()`. Verify that these finish without errors
   - Verify that these pass without seeing errors
   
6) Under the build tab, run 'Check'
   - Verify that only one warning arises
     - This warning is due to the [limitations](https://github.com/wch/R6/issues/3) of roxygen and R6 method documentation
     - This is the only warning/error/note that's allowed when merging to master

## Configure git

1) Open RStudio -> Git tab -> Gear Icon -> Shell

2) Set up your email and user name (for [proper attribution](https://help.github.com/articles/setting-your-username-in-git/))
   - `git config user.name "Billy Everyteen`
   - `git config --global user.email "your_email@example.com"`

3) Configure line endings via `git config core.autocrlf true`

4) Make git case-sensitive via `git config core.ignorecase false`

5) If you use a personal email for github, and would rather have notifications go to your Health Cataylst email
   - See [here]()https://github.com/settings/notifications -> Notification email -> Custom routing

6) Set up SSH so you can push without typing password (optional, but nice)
   - See [here](https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/), [here](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/), and then [here](https://help.github.com/enterprise/11.10.340/user/articles/changing-a-remote-s-url/)
