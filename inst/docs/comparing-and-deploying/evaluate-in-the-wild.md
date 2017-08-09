# Evaluate production performance via `generateAUC`

## What is this?

After [deploying a model](http://healthcareai-r.readthedocs.io/en/latest/comparing-and-deploying/deploy/) and running nightly predictions through a full cycle (i.e., ~30 days if you're predicting 30-day readmissions), it's time to evaluate your model in the wild.

This is done because production performance can vary significantly from the performance that was seen on past data (i.e., in the [development step](http://healthcareai-r.readthedocs.io/en/latest/comparing-and-deploying/compare/)).

## What do I need for this?

Two columns:

1. The predicted probabilities
2. The corresponding true outcomes (i.e., Y or N)

## How it works

- __Arguments__:
    - __predictions__: A vector or column of predictions from a machine learning model.
    - __labels__: A vector or column of the true labels (can be T/F, 1/0, Y/N, etc). Must be the same length as predictions.
    - __aucType__: A string. Indicates AUC_ROC or AU_PR and can be "SS" or "PR". Defaults to SS.
    - __plotFlg__: Binary value controlling plots. Defaults to FALSE (no).
    - __allCutoffsFlg__: Binary value controlling list of all thresholds. Defaults to FALSE (no).
    
A simple example:
```r
library(healthcareai)

# Generate some fake data
# Example probablities
df <- data.frame(predictions = rep( seq(0,1,by=0.1), times=9))
# Example ground truth values
df[,'truth'] <- (runif(99,0,1)*df[,'predictions']) > 0.5

# Look at the data
head(df)

# generate the AUC
AUC <- generateAUC(predictions = df$predictions, 
                   labels = df$truth)
```

If that performance is significantly lower than what you found in the develop step, you may have to

1. Check your columns for any that might be populated after the target column
2. [Retrain](http://healthcareai-r.readthedocs.io/en/latest/comparing-and-deploying/compare/) and check that your performance on past data is close to what it was previously
3. [Create predictions](http://healthcareai-r.readthedocs.io/en/latest/comparing-and-deploying/deploy/) for a full cycle (i.e., until you know whether patients were readmitted, etc)

If the performance found via `generateAUC` is close to what you found in the develop step

1. Show your work to the parties involved (i.e, the clinicians)
2. Describe the performance (perhaps with a plot from `generateAUC`)
3. Finalize your model and visualization after gathering feedback
4. Check-in on model performance monthly, or automate a more frequent check via `generateAUC`


