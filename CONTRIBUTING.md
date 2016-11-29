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
- If using macOS, the Xcode install [above](##Set up Git) has you covered
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
  
