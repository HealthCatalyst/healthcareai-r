# Deploy models and make new predictions via `DeployLasso`, `DeployRandomForest`, or `DeployLinearMixedModel`

# What is this?

These classes let one deploy custom models on varied datasets via the following workflow: 

1. Using the model [development](compare.md) functions, you found a model that performs well and saved it automatically. 
2. Now, run the deploy methods however often you need to load the model and generate predictions for new people/encounters. 
3. Retrain the model whenever significant changes occur with the data (perhaps quarterly) using model development.

One can do both classification (ie, predict Y or N) as well as regression (ie, predict a numeric field).

## Is any dataset ready for model creation and deployment?

Nope. It'll help if you can follow these guidelines:

* Don't use 0 or 1 for the independent variable when doing classification. Use Y/N instead. The IIF function in T-SQL may help here.
* Unlike the [development step](compare.md) (which you should have already completed), you now only need to select test rows in your query.
* Predictions on test rows can be output to a dataframe or directly to an MSSQL table. If using a table, one has to create the table to receive the predicted values. You can work in SSMS (or SAMD, for those using Health Catalyst products):
    - Create these tables when doing classification or regression, respectively:
        
```SQL
CREATE TABLE [SAM].[dbo].[HCRDeployClassificationBASE] (
[BindingID] [int] ,
[BindingNM] [varchar] (255),
[LastLoadDTS] [datetime2] (7),
[PatientEncounterID] [decimal] (38, 0),
[PredictedProbNBR] [decimal] (38, 2),
[Factor1TXT] [varchar] (255),
[Factor2TXT] [varchar] (255),
[Factor3TXT] [varchar] (255)
)

CREATE TABLE [SAM].[dbo].[HCRDeployRegressionBASE] (
[BindingID] [int],
[BindingNM] [varchar] (255),
[LastLoadDTS] [datetime2] (7),
[PatientEncounterID] [decimal] (38, 0),
[PredictedValueNBR] [decimal] (38, 2),
[Factor1TXT] [varchar] (255),
[Factor2TXT] [varchar] (255),
[Factor3TXT] [varchar] (255)
)
```

## How can I improve my model performance? 

Note these preprocessing steps should first be tested and found useful in the [development step](compare.md).

* If you have lots of NULL values, you may want to turn on imputation via the `impute` argument (see below).
* If you have lots of NULL cells and your data is longitudinal, you may want to try [GroupedLOCF](/model-pre-processing/longitudinal-imputation).
* If you think the phenomenon you're trying to predict has a seasonal or diurnal component, you may need some [feature engineering](/model-pre-processing/seasonality-handling).
* If your data is longitudinal, you may want to try the `LinearMixedModelDeployment` (detailed below).

## Step 1: Pull in the data via ``selectData``

- __Return__: a data frame that represents your data.

- __Arguments__:
    - __server__: a server name. You'll pull data from this server.
    - __database__: a database name. You'll pull data from this database.

```r
library(healthcareai)
library(RODBC)

connection.string = "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

# This query should pull only rows for training. They must have a label.
query = "
SELECT
[PatientEncounterID]
,[PatientID]
,[SystolicBPNBR]
,[LDLNBR]
,[A1CNBR]
,[GenderFLG]
,[ThirtyDayReadmitFLG]
FROM [SAM].[dbo].[HCRDiabetesClinical]
"

df <- selectData(connection.string, query)
head(df)
str(df)
```

Note: if you want a CSV example (ie, an example that you can run as-is), see the built-in docs:
```r
library(healthcareai)
?healthcareai
```

## Step 2: Develop and compare several models

Set up your parameters via `SupervisedModelDevelopmentParams`

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

Create the models via the `LassoDevelopment` or `RandomForestDevelopment` algorithms.

```r
# Run Lasso
Lasso <- LassoDevelopment$new(p)
Lasso$run()

# Run Random Forest
rf <- RandomForestDevelopment$new(p)
rf$run()
```

##Step 3: Deploy the model

Specify a deploy set

```r
#specify a deploy set
dfDeploy <- df[951:1000,]

```

Set deployment parameters via `SupvervisedModelDeploymentParams`

```r
p2 <- SupervisedModelDeploymentParams$new()
p2$type <- "classification"
p2$df <- dfDeploy
p2$grainCol <- "PatientEncounterID"
p2$predictedCol <- "ThirtyDayReadmitFLG"
p2$impute <- TRUE
p2$debug <- FALSE
p2$cores <- 1
```

Since random forest performed better in step 2, we will choose to deploy the random forest model.

```r
dL <- RandomForestDeployment$new(p2)
dL$deploy()
dfOut <- dL$getOutDf()
dfOut
```

## Full example code for reading from and pushing to SQL Server

```r
#### Classification example using SQL Server data ####
# This example requires you to first create a table in SQL Server.
# If you want to write output to a non-default schema, please see the section
# below, titled: "Deploying and Writing to non-default schemas in SQL Server".
# If you prefer to not use SAMD, execute this in SSMS to create output table:
# CREATE TABLE dbo.HCRDeployClassificationBASE(
#   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#   PatientEncounterID int, <--change to match inputID
#   PredictedProbNBR decimal(38, 2),
#   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
# )

## 1. Loading data and packages.
ptm <- proc.time()
library(healthcareai)
connection.string <- "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"
query <- "
SELECT
[PatientEncounterID] --Only need one ID column for random forest
,[SystolicBPNBR]
,[LDLNBR]
,[A1CNBR]
,[GenderFLG]
,[ThirtyDayReadmitFLG]
FROM [SAM].[dbo].[HCRDiabetesClinical]
"
df <- selectData(connection.string, query)
# Partition develop and deploy data
dfDeploy <- df[951:1000,]

## 2. Train and save the model using DEVELOP
print('Historical, development data:')
str(df)
set.seed(42)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "classification"
p$impute <- TRUE
p$grainCol <- "PatientEncounterID"
p$predictedCol <- "ThirtyDayReadmitFLG"
p$debug <- FALSE
p$cores <- 1
# Run RandomForest
RandomForest <- RandomForestDevelopment$new(p)
RandomForest$run()

## 3. Load saved model and use DEPLOY to generate predictions. 
print('Fake production data:')
str(dfDeploy)
p2 <- SupervisedModelDeploymentParams$new()
p2$type <- "classification"
p2$df <- dfDeploy
p2$grainCol <- "PatientEncounterID"
p2$predictedCol <- "ThirtyDayReadmitFLG"
p2$impute <- TRUE
p2$debug <- FALSE
p2$cores <- 1
dL <- RandomForestDeployment$new(p2)
dL$deploy()
dfOut <- dL$getOutDf()
writeData(MSSQLConnectionString = connection.string,
          df = dfOut,
          tableName = 'HCRDeployClassificationBASE')
print(proc.time() - ptm)
```

## Full example code for reading (and pushing predictions to) a CSV

Start with the arguments. You'll want to add

```r
#### Classification Example using csv data ####

## 1. Loading data and packages.
ptm <- proc.time()
library(healthcareai)
# setwd('C:/Yourscriptlocation/Useforwardslashes') # Uncomment if using csv
# Can delete this line in your work
csvfile <- system.file("extdata", 
                       "HCRDiabetesClinical.csv", 
                       package = "healthcareai")
# Replace csvfile with 'path/file'
df <- read.csv(file = csvfile, 
               header = TRUE, 
               na.strings = c("NULL", "NA", ""))
df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed remove this column
# Partition develop and deploy data
dfDeploy <- df[951:1000,]

## 2. Train and save the model using DEVELOP
print('Historical, development data:')
str(df)
set.seed(42)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "classification"
p$impute <- TRUE
p$grainCol <- "PatientEncounterID"
p$predictedCol <- "ThirtyDayReadmitFLG"
p$debug <- FALSE
p$cores <- 1
# Run RandomForest
RandomForest <- RandomForestDevelopment$new(p)
RandomForest$run()

## 3. Load saved model and use DEPLOY to generate predictions. 
print('Fake production data:')
str(dfDeploy)
p2 <- SupervisedModelDeploymentParams$new()
p2$type <- "classification"
p2$df <- dfDeploy
p2$grainCol <- "PatientEncounterID"
p2$predictedCol <- "ThirtyDayReadmitFLG"
p2$impute <- TRUE
p2$debug <- FALSE
p2$cores <- 1
dL <- RandomForestDeployment$new(p2)
dL$deploy()
dfOut <- dL$getOutDf()
head(dfOut)
# Write to CSV (or JSON, MySQL, etc) using plain R syntax
# write.csv(dfOut,'path/predictionsfile.csv')
print(proc.time() - ptm)
```

##Deploying and Writing to non-default schemas in SQL Server

As of the end of July 2017, there are some known issues with the packages `DBI`
and `odbc`.  These issues get in the way of those writing to non-default schemas
in SQL-Server. We hope to have the implementation of `DBI` and `odbc` fixed soon
but we are waiting on the developers of those resepctive packages to fix the 
issue. Links to the issues on github and stack overflow are  [here](https://stackoverflow.com/questions/45355885/odbc-dbi-in-r-will-not-write-to-a-table-with-a-non-default-schema-in-r?noredirect=1#comment77676303_45355885), [here](https://github.com/rstats-db/odbc/issues/91), and [here](https://github.com/rstats-db/DBI/issues/191). For now, we have created a work-around using the package `RODBC`.

__Note__: `RODBC` is difficult to install on Mac OS.  This work-around is mainly for
Windows users.  If Mac users can get`RODBC` installed on their machines, then the
same code will work, but the installation will not be trivial.

You may need to install RODBC using install.packages("RODBC"). Here is an 
example of the work-around. 

```r
#First, create a table in SQL Server using a non-default schema. The example 
#creates this table in the SAM database on localhost. You may also need to 
#create a new schema(Cardiovascular) in SSMS for this specific example to work.
#CREATE TABLE [Cardiovascular].[TestTable](
#[a] [float] NULL,
#[b] [float] NULL,
#[c] [varchar](255) NULL)

#install the RODBC pacakge onto your machine. You only need to do this one time.
install.packages("RODBC")
#load the package
library(RODBC)

#create a connection to work with
con <- RODBC::odbcDriverConnect('driver={SQL Server};
                              server=localhost;
                              database=SAM;
                              trusted_connection=true')

#build a df to write to SQL Server. df columns names must match the SSMS table.
df <- data.frame(a = c(10, 20, 30),
                 b = c(20, 40, 60),
                 c = c("oneT", "twoT", "threeT"))

#write the df to the table                 
RODBC::sqlSave(con, df, "Cardiovascular.TestTable", append = TRUE, 
                rownames = FALSE)
                
#verify that the table was written to
confirmDf <- sqlQuery(con, 'select * from Cardiovascular.TestTable')
head(confirmDf)
```

Note: if you need to see the built-in docs (which are always up-to-date):
```r
library(healthcareai)
?healthcareai
```
