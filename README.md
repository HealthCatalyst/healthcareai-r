# HCRTools

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/levithatcher/HCRTools?branch=master&svg=true)](https://ci.appveyor.com/project/levithatcher/HCRTools)


The aim of `HCRTools` is to make it easy to do data science with healthcare 
data. The package has three main goals:

-  Allow one to easily create models based on tabular data, and deploy a best
model that pushes predictions to SQL Server.

-  Provide tools related to data cleaning, manipulation, and imputation.

-  Make data exploration easy via interactive Shiny apps. This is not only for 
data residing as-is in SQL Server, but also to provide interactivity
with alternative-scenario predictions made from a model (which helps with
treatment protocol optimization).

To get started, check out this [notebook](notebooks/HCRToolsExample1.ipynb)

After finding a model that works, to push predictions to SQL Server, see this
[notebook](notebooks/HCRToolsExample2.ipynb)
