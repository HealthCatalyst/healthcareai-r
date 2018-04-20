# healthcareai 2.0.0

A whole new architecture featuring a simpler API, more rigor under the hood, and attractive plots. 

# healthcareai 1.2.4

- Patch to conform to CRAN policy of not writing to inst/
- Patch to maintain compatibility with `ranger` and `caret`
- Import `methods` to maintain functionality across environments
- Clean up of variation-exploration functions

# healthcareai 1.2.0

#### Added
- *Limone* -- a [lime](https://www.oreilly.com/learning/introduction-to-local-interpretable-model-agnostic-explanations-lime)-like model interpretation tool.
    - Called via `getProcessVariablesDf`
    - See examples at the end of the help files for `RandomForestDeployment` and `LassoDeployment` for usage details

# healthcareai 1.1.0

#### Added
- Deploy now saves information about the model and deployment as an attribute of the output dataframe. This information is written to a log file in the working directory.
- `skip_on_not_appveyor` will skip a unit test unless it's being run on Appveyor.

#### Changed
- Unit tests involving MSSQL now only run on Appveyor.

#### Removed
- `skip_if_no_mssql` isn't needed as a test utility anymore.


# healthcareai 1.0.0

#### Added
- Multiclass functionality with XGBoost is supported using `XGBoostDevelopment` and `XGBoostDeployment`. 
- K-means clustering is supported using `KmeansClustering`.
- `findVariaion` will return groups with the highest variation of a chosen target measure within a data set.
- `variationAcrossGroups` will plot a boxplot of variation between groups for a chosen target measure.

#### Changed
- `SupervisedModelDevelopment` now saves the model after training
- `SupervisedModelDeployment` no longer trains models. It only loads the model saved in `SupervisedModelDevelopment`. Predictions are made for all data.
- `imputeColumn` was replaced with `imputeDF`
- SQL tools now use a `DBI` backend. We support reading and writing to MSSQL and SQLite databases.
- SQL tools are now common functions used outside the algorithms.
- Model file documentation files now accurately reflect the available methods.

#### Removed
- `testWindowCol` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.
- `writeToDB` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.
- `destSchemaTable` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.

# healthcareai 0.1.12

#### Added
- Added getters for predictions `getPredictions()` in development (lasso, random forest, linear mixed model)
- Added getOutDf to each algorithm deploy file so predictions can go to CSV
- Added percentDataAvailableInDateRange, to eventually replace countPercentEmpty
- Added featureAvailabilityProfiler

#### Changed
- TimeStamp column predictive output is now local time (not GMT)

#### Fixed

# healthcareai 0.1.11

#### Added

- Added changelog
- Added travis.yml to prepare for CRAN release

#### Changed

- generateAUC now calls getCutOffs to give guidance on ideal cutoffs.
- getCutOffs now generates list of cutoffs and suggests ideal ones.
- API changes for both functions.
- calculatePerformance (model class method) now calls generateAUC

#### Fixed
- Bug fixes in example files concerning reproducability
