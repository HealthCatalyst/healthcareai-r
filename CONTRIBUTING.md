# Setting up your dev environment (to enable contributions)

## Set up R, IDE, and healthcare.ai

1) [Download](http://cran.us.r-project.org/) latest R and install

2) [Download](https://www.rstudio.com/products/rstudio/download3/) RStudio and install

3) Follow the healthcare.ai [install instructions](https://github.com/HealthCatalystSLC/HCRTools/blob/master/README.md)

## Set up Git (Windows-specific) - OSX instructions [here](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git#Installing-on-Mac)

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

