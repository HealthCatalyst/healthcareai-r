# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) 
and this project adheres to [Semantic Versioning](http://semver.org/).

## [1.2.1] - 2017-11-29

### Added
- `findVariation` now has a `wideOutput` argument that enables a tidy data frame to be returned. `wideOutput` defaults to TRUE, which preserves the old (non-tidy) behavior; FALSE produces a tidy data frame.
- `variationAcrossGroups` now leverages `ggplot2` for its boxplot, which produces reliably better graphics.
    - `plotBoxplot` is new logical argument that allows the user to turn off printing of the boxplot. Defaults to TRUE.
    - If `plotGroupDifferences` is TRUE, the chart is written to file in the working directory instead of printing on screen. This eliminates the need for an interactive session with the user pressing Enter to produce a plot.

## [1.2.0] - 2017-10-19

### Added
- *Limone* -- a [lime](https://www.oreilly.com/learning/introduction-to-local-interpretable-model-agnostic-explanations-lime)-like model interpretation tool.
    - Called via `getProcessVariablesDf`
    - See examples at the end of the help files for `RandomForestDeployment` and `LassoDeployment` for usage details

## [1.1.0] - 2017-10-11

### Added
- Deploy now saves information about the model and deployment as an attribute of the output dataframe. This information is written to a log file in the working directory.
- `skip_on_not_appveyor` will skip a unit test unless it's being run on Appveyor.

### Changed
- Unit tests involving MSSQL now only run on Appveyor.

### Removed
- `skip_if_no_mssql` isn't needed as a test utility anymore.


## [1.0.0] - 2017-08-02

### Added
- Multiclass functionality with XGBoost is supported using `XGBoostDevelopment` and `XGBoostDeployment`. 
- K-means clustering is supported using `KmeansClustering`.
- `findVariaion` will return groups with the highest variation of a chosen target measure within a data set.
- `variationAcrossGroups` will plot a boxplot of variation between groups for a chosen target measure.

### Changed
- `SupervisedModelDevelopment` now saves the model after training
- `SupervisedModelDeployment` no longer trains models. It only loads the model saved in `SupervisedModelDevelopment`. Predictions are made for all data.
- `imputeColumn` was replaced with `imputeDF`
- SQL tools now use a `DBI` backend. We support reading and writing to MSSQL and SQLite databases.
- SQL tools are now common functions used outside the algorithms.
- Model file documentation files now accurately reflect the available methods.

### Removed
- `testWindowCol` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.
- `writeToDB` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.
- `destSchemaTable` is no longer a param in `SupervisedModelDeployment` or used in the algorithms.


## [0.1.12] - 2017-05-08

### Added
- Added getters for predictions `getPredictions()` in development (lasso, random forest, linear mixed model)
- Added getOutDf to each algorithm deploy file so predictions can go to CSV
- Added percentDataAvailableInDateRange, to eventually replace countPercentEmpty
- Added featureAvailabilityProfiler

### Changed
- TimeStamp column predictive output is now local time (not GMT)

### Fixed

## [0.1.11] - 2017-03-02

### Added

- Added changelog
- Added travis.yml to prepare for CRAN release

### Changed

- generateAUC now calls getCutOffs to give guidance on ideal cutoffs.
- getCutOffs now generates list of cutoffs and suggests ideal ones.
- API changes for both functions.
- calculatePerformance (model class method) now calls generateAUC

### Fixed
- Bug fixes in example files concerning reproducability

[Unreleased]: https://github.com/HealthCatalystSLC/healthcareai-r/compare/v0.1.11...HEAD
[0.1.11]: https://github.com/HealthCatalystSLC/healthcareai-r/releases/tag/v0.1.11-beta
