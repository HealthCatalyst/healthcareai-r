# HCRTools

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/guud9xoxk62rdngr?svg=true)](https://ci.appveyor.com/project/levithatcher/hcrtools/branch/master)


The aim of `HCRTools` is to make it easy to do data science with healthcare 
data. The package has two main goals:

-  Allow one to easily create models based on tabular data, and deploy a best
model that pushes predictions to SQL Server.

-  Provide tools related to data cleaning, manipulation, and imputation.

To install:

- Open RStudio or RGui

- Install devtools via the console: install.packages('devtools')

- Generate github personal access token (PAT)
  - Naviagate here: https://github.com/settings/tokens/
  - Check the top-level repo box
  - Store token in secure place
  
- Use this token to download and install HCRTools via this command in the
console: 

library(devtools)
devtools::install_github(repo='HealthCatalystSLC/HCRTools',user='username',auth_token='yourtoken')

To get started, check out this [notebook.](inst/notebooks/HCRToolsExample1.ipynb)

After finding an accurate model, see this
[notebook](inst/notebooks/HCRToolsExample2.ipynb) to push predictions to SQL Server.
