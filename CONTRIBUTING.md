# Setting up your dev environment (to enable contributions)

## Set up R, IDE, and healthcare.ai

1) Download latest R for Windows and install

2) Download RStudio for Windows and install

3) Follow the healthcare.ai [install instructions](https://github.com/HealthCatalystSLC/HCRTools/blob/master/README.md)

## Set up Git (Windows-specific)

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

## 
