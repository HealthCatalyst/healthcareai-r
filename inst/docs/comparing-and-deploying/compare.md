# Create and compare models via `LassoDevelopment`, `RandomForestDevelopment`, and `LinearMixedModelDevelopment`

# What is this?

These classes let one create and compare custom models on varied datasets.

One can do both classification (ie, predict Y or N) as well as regression (ie, predict a numeric field, like cost).

## Is any dataset ready for model creation?

Nope. It'll help if you can follow these guidelines:

* Don't use 0 or 1 for the independent variable when doing classification. Use Y/N instead. The IIF function in T-SQL may help here.
* Don't pull in test data in this step. In other words, to compare models, we don't need to worry about those rows that need a prediction quite yet.

## How can I improve my model performance?

* If you have lots of NULL cells and your data is longitudinal, you may want to try [GroupedLOCF](/model-pre-processing/longitudinal-imputation).
* If you think the phenomenon you're trying to predict has a seasonal or diurnal component, you may need some [feature engineering](/model-pre-processing/seasonality-handling).
* If your data is longitudinal, you may want to try the `LinearMixedModelDevelopment` (detailed below).

## Step 1: Pull in the data via `selectData`

- __Return__: a data frame that represents your data.
- __Arguments__:
    - __server__: a server name. You'll pull data from this server.
    - __database__: a database name. You'll pull data from this database.

```r
ptm <- proc.time()
library(healthcareai)
library(RODBC)

connection.string = "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

query = "
SELECT
[PatientEncounterID]
,[PatientID]
,[SystolicBPNBR]
,[LDLNBR]
,[A1CNBR]
,[GenderFLG]
,[ThirtyDayReadmitFLG]
,[InTestWindowFLG]
FROM [SAM].[dbo].[DiabetesClinical]
WHERE InTestWindowFLG = 'N'
"

df <- selectData(connection.string, query)
head(df)
```

Note: if you want a CSV example (ie, an example that you can run as-is), see the built-in docs:
```r
library(healthcareai)
?healthcareai
```

## Step 2: Set your parameters via ``SupervisedModelDevelopmentParams``

- __Return__: an object representing your specific configuration.
- __Arguments__:
    - __df__: a data frame. The data your model is based on.
    - __type__: a string. This will either be 'classification' or 'regression'.
    - __impute__: a boolean, defaults to FALSE. Whether to impute by replacing NULLs with column mean (for numeric columns) or column mode (for categorical columns).
    - __grainCol__: a string, defaults to None. Name of possible GrainID column in your dataset. If specified, this column will be removed, as it won't help the algorithm.
    - __predictedCol__: a string. Name of variable (or column) that you want to predict. 
    - __debug__: a boolean, defaults to FALSE. If TRUE, console output when comparing models is verbose for easier debugging.
    - __cores__: an int, defaults to 4. Number of cores on machine to use for model training.

```r
p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$type = 'classification'
p$impute = TRUE
p$grainCol = 'PatientEncounterID'
p$predictedCol = 'ThirtyDayReadmitFLG'
p$debug = FALSE
p$cores = 1
```

## Step 3: Create the models via the `LassoDevelopment` and `RandomForestDevelopment` algorithms.

```r
# Run Lasso
Lasso <- LassoDevelopment$new(p)
Lasso$run()

# Run Random Forest
rf <- RandomForestDevelopment$new(p)
rf$run()
```

## `LassoDevelopment` Details

This version of Lasso is based on the Grouped Lasso alogrithm offered by the [grpreg package](https://cran.r-project.org/web/packages/grpreg/grpreg.pdf). We prefer simple models to complicated ones, so for tuning the lambda regularization parameter, we use the 1SE rule, which means that we take the model with fewest coefficients, which is also within one standard error of the best model. This way, we provide guidance as to which features (ie, columns) should be kept in the deployed model. 

## `RandomForestDevelopment` Details

This version of random forest is based on the wonderful [ranger package](https://cran.r-project.org/web/packages/ranger/ranger.pdf).

## Full example code

```r
ptm <- proc.time()
library(healthcareai)
library(RODBC)

connection.string = "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

query = "
SELECT
 [PatientEncounterID]
,[PatientID]
,[SystolicBPNBR]
,[LDLNBR]
,[A1CNBR]
,[GenderFLG]
,[ThirtyDayReadmitFLG]
FROM [SAM].[dbo].[DiabetesClinical]
WHERE InTestWindowFLG = 'N'
"

df <- selectData(connection.string, query)
head(df)

df$PatientID <- NULL

set.seed(42)

p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$type = 'classification'
p$impute = TRUE
p$grainCol = 'PatientEncounterID'
p$predictedCol = 'ThirtyDayReadmitFLG'
p$debug = FALSE
p$cores = 1

# Run Lasso
Lasso <- LassoDevelopment$new(p)
Lasso$run()

# Run Random Forest
rf <- RandomForestDevelopment$new(p)
rf$run()

# For a given true-positive rate, get false-pos rate and 0/1 cutoff
Lasso$getCutOffs(tpr=.8)

print(proc.time() - ptm)
```

## `LinearMixedModelDevelopment` Details

This mixed model is designed for longitudinal datasets (ie, those that typically have more than one row per-person). The method is based on the lme4 package. It's not as computationally efficient as the random forest algorithm, so it's best to compare against the other algorithms on smaller datasets, and then scale up from there.

## Full example code for mixed-model longitudinal work

```r
# Example using SQL Server
ptm <- proc.time()
library(healthcareai)

connection.string = "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

query = "
SELECT
 [PatientEncounterID]
,[PatientID]
,[SystolicBPNBR]
,[LDLNBR]
,[A1CNBR]
,[GenderFLG]
,[ThirtyDayReadmitFLG]
FROM [SAM].[dbo].[DiabetesClinical]
"

df <- selectData(connection.string, query)
head(df)


set.seed(42)

p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$type = 'classification'
p$impute = TRUE
p$grainCol = 'PatientEncounterID'
p$personCol = 'PatientID'          # <- Specific to Mixed Models
p$predictedCol = 'ThirtyDayReadmitFLG'
p$debug = FALSE
p$cores = 1

# Create Mixed Model
lmm <- LinearMixedModelDevelopment$new(p)
lmm$run()

# For a given true-positive rate, get false-pos rate and 0/1 cutoff
Lasso$getCutOffs(tpr=.8)

print(proc.time() - ptm)
``` 

Note: if you want a CSV example (ie, an example that you can run as-is), see the built-in docs:
```r
library(healthcareai)
?healthcareai
```
