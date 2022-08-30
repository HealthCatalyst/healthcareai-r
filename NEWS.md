# healthcareai 2.5.1

#### Fixed
- Numerous bugs with recipes and other updated packages.

# healthcareai 2.5.0

#### Breaking changes 

- `healthcareai` now depends on dplyr 1.0.0 and tibble 3.0.0. You will need these versions or later. Various hidden changes were made to be compatible with these packages' [lastest breaking changes](https://github.com/tidymodels/recipes/blob/master/NEWS.md). 

#### Changed

- The `allow_parallel` parameter was removed to prevent nested parallelism.

# healthcareai 2.3.0

#### Breaking changes 

- `healthcareai` now depends on recipes 0.1.4 and caret 6.0.81. You will need these versions or later. Various hidden changes were made to be compatible with these packages' [lastest breaking changes](https://github.com/tidymodels/recipes/blob/master/NEWS.md). 

#### Changed

- `bagimpute` in `prep_data` now accepts `bag_trees` to specify the number of trees. This is updated to be compatible with [recipes 0.1.4](https://github.com/tidymodels/recipes/blob/master/NEWS.md).
- Local loaded `healthcareai` library versions now are saved to model objects.

# healthcareai 2.2.0

#### Added

- Support for models of outcomes with more than two classes (multiclass)
- Explore a model's logic with `explore`. Make counterfactual predictions across the most-important features in a model to see how those features influence predicted outcomes.
    + `plot` method to visualize a model's logic.
- Identify opportunities to improve a patient's outcome with Patient Impact Predictor, `pip`. Carefully specify variables and alternative values that exert causal influence on outcomes; then get recommended actions for a given patient with expected outcomes given the actions.
- Predict outcome groups based on how bad false alarms are relative to missed detections (`outcome_groups` argument to `predict`).
- Group predictions into risk groups using the `risk_groups` argument to `predict`.
    + `plot` support for outcome- and risk-group predictions.
- Get thresholds to split outcome classes to optimize various performance metrics with `get_thresholds`.
    + `plot` method to compare performance across metrics at various thresholds.
- `split_train_test` can keep multiple observations of an individual in the same split via the `grouping_col` argument.
- Replace values that represent missingness but have been interpreted by R as strings with `NA` with `make_na`.
    + If `missingness` finds any such strings it issues a warning with code that can be used to do the replacement.
- Add counts to factor levels with `rename_with_counts`.
- `summary.missingness` method for wide datasets with missingness in many columns.

#### Changed

- In `prep_data`, trigonometric transformations make circular features out of dates and times for more informative features in less-wide data frames.
- Fixed AUPR in `plot.model_class` and `summary.model_class`.
- Can specify performance metric to optimize in `machine_learn`.
- `missingness` is faster.
- Predict on XGBoost models now works for any column order in the new dataset.
- Regression prediction plots are plotted at 1:1 aspect ratio.
- `add_best_levels` works in deployment even if none of the columns to be created are present in the deployment observations.
- `prep_data` can handle logical features.
- `outcome` doesn't need to be re-declared in model training if it was specified in data prep.

#### Removed

- No longer support training models on un-prepped data.
- No longer support wrapping `caret`-trained models into a `model_list`.

# healthcareai 2.1.0

#### Added

- Identify values of high-cardinality variables that will make good features, even with multiple values per observation with `add_best_levels` and `get_best_levels`.
- glmnet for regularized linear and logistic regression.
- `interpret` and `plot.interpret` to extract glmnet estimates.
- XGBoost for regression and classification models.
- `variable_importance` returns random forest or xgboost importances, whichever model performs better.

#### Changed

- `predict` can now write an extensive log file, and if that option is activated, as in production, `predict` is a safe function that always completes; if there is an error, it returns a zero-row data frame that is otherwise the same as what would have been returned (provided `prep_data` or `machine_learn` was used).
- Control how low variance must be to remove columns by providing a numeric value to the `remove_near_zero_variance` argument of `prep_data`.
- Fixed bug in missingness that caused very small values to round to zero.
- Messages about time required for model training are improved.
- `separate_drgs` returns `NA` for complication when the DRG is missing.
- Removed some redundent training data from `model_list` objects.
- `methods` is attached on attaching the package so that scripts operate the same in Rscript, R GUI, and R Studio.
- Minor changes to maintain compatibility with `ggplot2`, `broom`, and `recipes`.

#### Removed

- Removed support for k-nearest neighbors
- Remove support for maxstat splitting rule in random forests

# healthcareai 2.0.0

A whole new architecture featuring a simpler API, more rigor under the hood, and attractive plots. 

# healthcareai 1.2.4

- Patch to conform to CRAN policy of not writing to inst/
- Patch to maintain compatibility with `ranger` and `caret`
- Import `methods` to maintain functionality across environments
- Clean up of variation-exploration functions

# healthcareai 1.2.0

#### Added
- *Limone* -- a [lime](https://www.oreilly.com/content/introduction-to-local-interpretable-model-agnostic-explanations-lime/)-like model interpretation tool.
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
