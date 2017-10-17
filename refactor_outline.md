# Development Guidelines

We're staying current with other R packages, so it's a good idea to update R and packages frequently. Update R from [CRAN](https://cran.r-project.org/) and then update packages with `update.packages(ask = FALSE)`. Updating packages will take a few minutes if you haven't done it recently.

We're using `strict` to enforce betteR practices, but RStudio [currently breaks](https://github.com/hadley/strict/issues/5) if we try to load the package automatically at startup, so for now, run `library(strict)` whenever you're developing.

We're abiding by the [tidyverse style guide](http://style.tidyverse.org/). 

Document using `roxygen2`. Be sure to read the [documentation section](http://style.tidyverse.org/code-documentation.html) of the style guide. 

- Keep examples succinct. In general, one example showing the minimal use of the function (with defaults) and one example showing a customized use should be adequate. Longer examples can be vignettes.

Use `testthat` for testing. Write tests before you write function code. Read the [testing chapter in Hadley's R packages](http://r-pkgs.had.co.nz/tests.html) about how to write good tests.

In general, each function should get its own file with the filename matching the function name. Note that subdirectories within `healthcareai-r/R` are not allowed. Verb function names are good, e.g. `find_correlations`, `predict`.

Exceptions to one-function-per-file:

- Class definition files should be called `class.R` and contain `class()` and `as.class()` constructor functions and a `is.class` test function. 
- Short, non-exported utility functions can go in the `utilities.R` file.
- Non-exported functions that will likely only be called by one exported function can go below the calling function in its file. This is useful to keep individual functions from getting too long. 

All old code has been moved into `R/depreciated`. If you are going to use a substantial amount of code from the old package, bring it back into `R` and rename it to current standards with something like `git mv R/depreciated/common-correlations R/find_correlations`. 

# Functions

## Profiling

*Much of this section is reworking existing functions to work better, put fewer requirements on the user (e.g. not needing to pre-sort a data frame, choose sensible defaults), conform to R standards (e.g. separate numeric and text columns in findVariation), and leverage tidyverse to enable piping, pretty ggplot plots, etc.*

Eventually, we should wrap this into a single `profile` function, optionally writing a notebook that contains the results.

These functions should generally return an object that has `print.`, `summary.`, and `plot.` methods, and have slots for whatever data the user might want to extract, e.g. `null_profiled$percent_missing`.

- `print.` -- what happens if you run the function without assignment; a very high level summary
- `summary.` -- a more detailed high level view
- `plot.` -- summarize results in a plot

### Profiling Functions

- Find correlations
	- This could identify numeric and low-cardinality categoricals and calculate correlations between them
    - `plot.` method could call `corrplot` or `ggpairs`
- Feature availability over time since admit, `featureAvailabilityProfiler`
- Null profiling, % missing in each column, optionally within dates as in `percentDataAvailableInDateRange`
- Find problems: Wrapper that calls several functions:
    + Look for strings like "null", "NULL", "na", etc.
    + look for numeric categories that should perhaps be categorical (only a few distinct entries)
    + look for categorical categories that should perhaps be numeric (mostly number-only entries with a few strings that force it to be category, probably strings like "null")
    + Look for high-cardinality categorical variables, like `returnColsWithMoreThanFiftyCategories` but more customizable
    + ID columns with no variance 
    + ID categorical columns with unique entries on every row
- find variation & variation across groups

## Data shaping

- Collapse multiple rows to "grain level". Call the following:
    + Identify varying columns within grain, i.e. when you collapse to one row per grainID, which columns are you going to have to do some feature engineering on, as opposed to those that are simply repeated within grain
    + Auto-engineer features while collapsing: min, max, earliest, latest, mean, variance

## Data Prep Pipeline

This should all happen through the [`recipes`](https://topepo.github.io/recipes/) API so that the pipeline can be captured and reused in deployment. `recipes` makes this easy, we just need to figure out what options we want to surface to the user and how to turn them into functional code.

These options don't all need to be available in the v2.0 release. In roughly descending order of priority:

- Imputation:
    + Categorical: impute "unknown", impute mode
    + Numeric: impute mean
    + Both: knn, bagged trees
    + Longitudinal: LOCF. Don't require the user to sort the data frame and lose the data.table dependency. 
        * This isn't available off the shelf in `recipes`, but it would be great if we can keep this in the `recipes` pipeline. Here is the [guide to defining new steps](https://topepo.github.io/recipes/articles/Custom_Steps.html). 
- [timestamp -> features](https://topepo.github.io/recipes/reference/step_date.html) 
    + And [holidays](https://topepo.github.io/recipes/reference/)step_holiday.html)
- [Group low-frequency categories](https://topepo.github.io/recipes/reference/step_other.html)
- Impact Coding, see Ethans PR #653 -- This should really be worked into the `recipes` pipeline so that it can be re-deployed at prediction time. Here is the [guide to defining new recipe steps](https://topepo.github.io/recipes/articles/Custom_Steps.html). 
- [Create interactions](https://topepo.github.io/recipes/reference/step_interact.html) -- this shouldn't be done for tree-based models that can get at interactions other ways, but it can be very useful for regression based models. Consider giving the user the option to specify `interaction_depth = c(1, 2, 3)`, warn them if that would create a very large table, and use the interaction-including dataset only for regression based models.  
- [ordered categories to numerics](https://topepo.github.io/recipes/reference/step_ordinalscore.html)
- Scaling & centering
- [PCA](https://topepo.github.io/recipes/reference/step_pca.html)

Note that the order in which we apply these steps matters. [This order](https://topepo.github.io/recipes/articles/Ordering.html) looks pretty reasonable.

## Training

### Setup

Take user inputs and prepare them for the cross validation pipeline. User input options:

- Required
    + data
    + predictCol
- Optional 
    - formula
    - type (classification, regression, etc.)
    + Algorithms to be used
    + Cross validation details
        * Number of folds
        * Number of repeats (or do we not want to offer repeated CV?)
        * Hyperparameter search details. One of:
            - Number of combinations to try, or
            - Hyperparameters to tune over, or
            - Hyperparameters to tune over + number of values to try for each, or
            - Hyperparameters to tune over + values to use (e.g. I know I want 200 trees, but tune over mtry = 10, 30, or 70)
    + Performance metric (AUROC, AUPR, accuracy or MRSE, MAE, etc.)
    + Loss function for classification scored by accuracy

### Cross Validation

Identify best hyper-parameter values for each model to be trained. Return a `model_list` object with child class `regression_ist`, `multiclass_list`, etc. (and parent class `list`).

- Note that for some models this will involve retraining the model for each combination of hyperparamter values in each CV fold (automated by caret), while other models can accomplish hyperparameter optimization analytically (see both of the answers [here](https://stats.stackexchange.com/questions/69638/) and the second paragraph in [this caret documentation page](http://topepo.github.io/caret/random-hyperparameter-search.html))
- Keep out-of-fold predictions for the best hyperparameters in the model_list object returned from CV

#### model_list Methods

- `print`: which model did best and what was its score
- `summary`: scores of all models
- `plot`: bar/dot chart of model scores. 
- `plot_ROC` & `plot_PR` for `classification_list` objects (probably leveraging existing packages)
    + Plot all model's curves and provide AUCs in legend
    + Those functions should also have methods to take a vector of predictions and a vector of outcomes
- `plot_predictions` for `regression_list`?
    + If just model list, plot predictions vs. actual
    + Provide argument for predictor. If provided plot residuals vs the predictor
    + Provide argument for group(s). Color points by first group, shape points by second group.

### Evaluate

Take a list of models from CV and score them based on an optional user-selected metric (we'll choose defaults) using the out-of-fold predictions from cross validation. Use this by default to choose a model to use for prediction.

## Predict

In general, `predict` should take a model or model_list and a dataframe (`newdata`) and return predictions. It doesn't change the model object. 

* Classification: should have switch for class vs probability predictions. For former should have these options:
    - maximize accuracy based on training (default)
    - User provided threshold
    - User provided loss function (i.e. ratio of cost of false positive:false negative)
* Regression: should have option to return, in addition to predictions, confidence or prediction intervals (lasso/ridge will provide both. What does RF / other tree based models provide?)

### Limone

- See Yannick's PR #650.
- I think this should be called after/separately from `predict` rather than being a switch to turn on during predict

## Database interaction

`selectData` seems fine as is to me. Never used `writeData` but I assume it's good too. Anything to do here?
