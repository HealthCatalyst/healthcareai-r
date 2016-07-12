# HCRTools

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/levithatcher/hcrtools?branch=master&svg=true)](https://ci.appveyor.com/project/levithatcher/hcrtools)


The aim of `HCRTools` is to make it easy to do data science with healthcare 
data. The package has two main goals:

-  Allow one to easily create models based on tabular data, and deploy a best
model that pushes predictions to SQL Server.

-  Provide tools related to data cleaning, manipulation, and imputation.

To install:

- Click on 'Clone or download' above and download the zip file

- In RStudio's console, type setwd('C:/Path/To/Zip')

- Then type install.packages('HCRTools_0.1.7.zip', repos=NULL, type='binary')

To get started, check out this [notebook.](inst/notebooks/HCRToolsExample1.ipynb)

After finding an accurate model, see this
[notebook](inst/notebooks/HCRToolsExample2.ipynb) to push predictions to SQL Server.
