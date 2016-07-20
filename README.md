# HCRTools

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/guud9xoxk62rdngr?svg=true)](https://ci.appveyor.com/project/levithatcher/hcrtools/branch/master)
 

The aim of `HCRTools` is to make it easy to do data science with healthcare 
data. The package has two main goals:

-  Allow one to easily create models based on tabular data, and deploy a best
model that pushes predictions to SQL Server.

-  Provide tools related to data cleaning, manipulation, and imputation.

Installation:

- Open RGui (which comes with R) or RStudio (which is recommended and downloaded separately)

- Install prerequisites:
```R
install.packages(c('caret','data.table','doParallel','e1071','grpreg','lubridate','pROC','R6','ranger','ROCR','RODBC'
),repos = "https://cran.cnr.berkeley.edu/")
```

- Install devtools via the console: 
```R
install.packages('devtools')
```

- Generate a github token
  - Navigate [here](https://github.com/settings/tokens) and click on 'Generate a 
  personal access token'
  - Check the top-level repo box
  - Store token in secure place
  
- Use your username/token to download and install HCRTools via the console: 

```R 
library(devtools)
devtools::install_github(repo='HealthCatalystSLC/HCRTools',user='yourusername',auth_token='yourtoken')
```

Getting started:

- To create a couple models on your data, check out this [notebook.](inst/notebooks/HCRToolsExample1.ipynb)

- After finding an accurate model, see this
[notebook](inst/notebooks/HCRToolsExample2.ipynb) to push predictions to SQL 
Server.
