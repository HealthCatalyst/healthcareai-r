# Save and deploy models via `DeployLasso`, `DeployRandomForest`, or `DeployLinearMixedModel`

# What is this?

These classes let one save and deploy custom models on varied datasets via the following workflow: 

1. Using the model [development](compare.md) functions, you found a model that performs well. 
2. Now, you train and save model on your entire dataset (with the `useSavedModel` argument set to FALSE). 
3. Next, flip the `useSavedModel` argument to TRUE and rerun the script however often you need to generate new predictions. 
    - Now that you're using a saved model, you're just running new people/encounters against the saved model to generate predictions.  
4. Retrain the model whenever significant changes occur with the data (perhaps quarterly) by flipping the `useSavedModel` to FALSE and go to step 3. 

One can do both classification (ie, predict Y or N) as well as regression (ie, predict a numeric field).

## Is any dataset ready for model creation and deployment?

Nope. It'll help if you can follow these guidelines:

* Don't use 0 or 1 for the independent variable when doing classification. Use Y/N instead. The IIF function in T-SQL may help here.
* Create a column thath as `Y` for those rows in the training set and `N` for those rows in the test set. Think of the test set as those people or enounters that need a prediction. This column can be called InTestWindow. 
* Unlike the [development step](compare.md) (which you should have already completed), you should now pull in both training and test rows in your query. 
* One has to create a table to receive the predicted values. You can work in SSMS (or SAMD, for those using Health Catalyst products):
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
* If your data is longitudinal, you may want to try the `LinearMixedModelDeployment`` (detailed below).

## Step 1: Pull in the data via ``selectData``

- __Return__: a data frame that represents your data.

- __Arguments__:
    - __server__: a server name. You'll pull data from this server.
    - __database__: a database name. You'll pull data from this database.

```r
library(healthcareai)

connection.string = "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

query = "
SELECT
[OrganizationLevel]
,[InTestWindowFLG]
,[MaritalStatus]
,[Gender]
,IIF([SalariedFlag]=0,'N','Y') AS SalariedFlag
,[VacationHours]
,[SickLeaveHours]
FROM [AdventureWorks2012].[HumanResources].[Employee]
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

## Step 2: Set your parameters via `SupervisedModelParameters`

- __Return__: an object representing your specific configuration.
- __Arguments__:
    - __df__: a data frame. The data your model is based on.
    - __type__: a string. This will either be 'classification' or 'regression'.
    - __impute__: a boolean, defaults to FALSE. Whether to impute by replacing NULLs with column mean (for numeric columns) or column mode (for categorical columns).
    - __grainCol__: a string, defaults to None. Name of possible GrainID column in your dataset. If specified, this column will be removed, as it won't help the algorithm.
    - __testWindowCol__: a string. Name of utility column used to indicate whether rows are in train or test set. Recall that test set receives predictions.
    - __predictedCol__: a string. Name of variable (or column) that you want to predict. 
    - __debug__: a boolean, defaults to FALSE. If TRUE, console output when comparing models is verbose for easier debugging.
    - __useSavedModel__: a boolean, defaults to FALSE. If TRUE, use the model that has been saved to disk in the current working directory (WD). If FALSE, save a new model to disk in the current WD. Use `getwd()` in the console to check WD.
    - __cores__: an int, defaults to 4. Number of cores on machine to use for model training.
    - __sqlConn__: a string. Specifies the driver, server, database, and whether you're using a trusted connection (which is preferred).
    - __destSchemaTable__ : a string. Denotes the output schema and table (separated by a period) where the predictions should be pushed.

```r
p <- DeploySupervisedModelParameters$new()
p$df = df
p$type = 'classification'
p$impute = TRUE
p$grainCol = 'GrainID'
p$testWindowCol = 'InTestWindow'
p$predictedCol = 'SalariedFlag'
p$debug = FALSE
p$useSavedModel = FALSE
p$cores = 1
p$sqlConn = connection.string
p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'
```

## Step 3: Create the models via the `DeployLasso` or `DeployRandomForest` algorithms.

```r
# Run Lasso (if that's what performed best in the develop step)
dL <- LassoDeployment$new(p)
dL$deploy()

# Or run RandomForest (if that's what performed best in the develop step)
dL <- RandomForestDeployment$new(p)
dL$deploy()

# Or run Linear Mixed Model (if that's what performed best in the develop step)

p$personCol = 'PatientID' # Change to your PatientID col
lMM <- LinearMixedModelDeployment$new(p)
lMM$deploy()
```
## Full example code

```r
ptm <- proc.time()
library(healthcareai)

connection.string = "
driver={SQL Server};
server=localhost;
database=AdventureWorks2012;
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

# Remove unnecessary columns
df$PatientID <- NULL

p <- SupervisedModelDeploymentParams$new()
p$type = 'classification'
p$df = df
p$grainCol = 'PatientEncounterID'
p$testWindowCol = 'InTestWindowFLG'
p$predictedCol = 'ThirtyDayReadmitFLG'
p$impute = TRUE
p$debug = FALSE
p$useSavedModel = FALSE
p$cores = 1
p$sqlConn = connection.string
p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'

# If Lasso was more accurate in the dev step
dL <- LassoDeployment$new(p)
dL$deploy()

# If Random Forest was more accurate in the dev step
#dL <- RandomForestDeployment$new(p)
#dL$deploy()

print(proc.time() - ptm)
```

Relevant example code:

```
p <- SupervisedModelParameters$new()
p$df = df
p$type = 'classification'
p$impute = TRUE
p$grainCol = 'PatientEncounterID' # This grain of the dataset (required)
p$personCol = 'PatientID'         # This represents the person (required)
p$predictedCol = 'HighA1C'
p$debug = FALSE
p$cores = 1

lMM <- LinearMixedModelDeployment$new(p)
lMM$deploy()
```

Note: if you want a CSV example (ie, an example that you can run as-is), see the built-in docs:
```r
library(healthcareai)
?healthcareai
```