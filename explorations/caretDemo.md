caret & recipes demo
================
Michael Levy <michael.levy@healthcatalyst.com>
2017-10-05

I'm thinking we should use caret. Why I've changed my thinking about this:

1.  Taylor Miller used the term battle-tested to describe the methods we use, and that has been bouncing through my head all day. `caret` is battle-tested. It's been around for a long time and has been through several refactors.
2.  It does a lot of what we want to do. We can wrap this stuff instead of reinventing it, and therefore focus on our value proposition (which I see as lowering the burden to entry of ML via healthcare specific functionality and providing the simplest possible API to ML)
3.  The author has made a helper package, [`recipes`](https://topepo.github.io/recipes/index.html) that would allow us to capture pre-processing steps like imputation and normalization and apply them identically in development and deployment.
4.  caret is well liked by the community. I still think the documentation is sub-par, but the fact that smart, experienced people like it makes me think I never get over the caret-specific learning curve. Eduardo Arino de la Rubia, who gave the *R/Python is the best* talk Taylor posted, [said](https://www.youtube.com/watch?v=YmHyAHkjX_A) "This is probably a little extreme, but I believe caret is one of R's genuine competitive advantages..."
5.  It is extensible. So while it doesn't have native support for xgb, [we can still use xgb in the caret framework](https://topepo.github.io/caret/using-your-own-model-in-train.html).
6.  It will keep us on the rails of modern R modeling. Kuhn, the developer of caret, has Hadley working on the `recpies` helper package, which is a good signal.
7.  It has a [ton of models](http://topepo.github.io/caret/available-models.html), any of which would be easy for us to add support for.

Demo
====

This obviously doesn't cover everything, but it shows how the packages work. I've comments to document what is `caret`, `recipes`, and `tidyverse` respectively.

Load packages and data and split into train and test sets:

``` r
library(tidyverse)
library(recipes)
library(caret)

csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "healthcareai")
df <- read.csv(file = csvfile, header = TRUE, na.strings = c("NULL", "NA", ""))
# tidyverse:
df <- replace_na(df, list(ThirtyDayReadmitFLG = "N"))  # Quickly fill in missing
# outcomes; impute on the predictors later

# caret:
trainRows <- createDataPartition(df$ThirtyDayReadmitFLG, 1, .9)$Resample1
# tidyverse:
trainDF <- slice(df, trainRows)
testDF <- slice(df, -trainRows)
```

Define a "recipe" of data prep:

``` r
# recipes:
# This defines the pipeline but doesn't manipulate the data
myRecipe <- 
  trainDF %>%
  recipe(ThirtyDayReadmitFLG ~ SystolicBPNBR + LDLNBR + A1CNBR + GenderFLG) %>%
  step_knnimpute(all_predictors(), K = 3) %>% # Also has bagged trees imputation, and mean and mode
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) # Here, could just do step_dummy(GenderFLG)

myRecipe
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          4
    ## 
    ## Steps:
    ## 
    ## 3-nearest neighbor imputation for all_predictors()
    ## Centering for all_numeric()
    ## Scaling for all_numeric()
    ## Dummy variables from all_nominal(), -all_outcomes()

Apply the recipe to the training data to create a model matrix

``` r
# This does calculations needed to manipulate the data (e.g. determine impute values)
myRecipe <- prep(myRecipe)
```

    ## step 1 knnimpute training 
    ## step 2 center training 
    ## step 3 scale training 
    ## step 4 dummy training

``` r
# Now we can apply those transformations to any dataset (by default the one defined on,
# but also the deployment dataset), optionally `prep`ing on the new dataset to 
# calculate new imputation values, e.g.
modelMatrix <- bake(myRecipe)
head(modelMatrix)
```

    ## # A tibble: 6 x 4
    ##   SystolicBPNBR     LDLNBR    A1CNBR GenderFLG_M
    ##           <dbl>      <dbl>     <dbl>       <dbl>
    ## 1     0.7090418  1.3931842 -1.690200           1
    ## 2     0.1567134  1.8966976 -1.005033           1
    ## 3     0.8273979  1.2871813 -1.861492           1
    ## 4     1.5375344 -0.4618652 -1.604554           1
    ## 5     1.4191783  0.9426722 -1.005033           1
    ## 6     1.5769864 -1.0978822 -1.861492           1

Define cross-validation details using random hyperparameter search. There is a lot of functionality on offer around hyperparameter tuning and model selection.

``` r
# caret:
# define model-agnostic details for hyperparameter tuning
cvDetails <- 
  trainControl(method = "cv",  # Can do repeat cv and bootstrapping just as easily
               number = 5, 
               classProbs = TRUE,  # Choose an optimal threshold and make Y/N calls
               summaryFunction = twoClassSummary,  # Lots of options here
               search = "random"  # or grid or modest support for fancier methods
  )
```

Train a model and inspect

``` r
rfFit <- train(x = modelMatrix, 
               y = trainDF$ThirtyDayReadmitFLG,
               metric = "ROC",
               trControl = cvDetails,
               tuneLength = 5  # How many hyperparameter combos to try
               )
methods(class = class(rfFit))  # Lots of methods available for us. Eg:
```

    ##  [1] confusionMatrix densityplot     fitted          ggplot         
    ##  [5] histogram       levels          plot            predict        
    ##  [9] predictors      print           residuals       stripplot      
    ## [13] summary         update          varImp          xyplot         
    ## see '?methods' for accessing help and source code

``` r
confusionMatrix(rfFit)
```

    ## Cross-Validated (5 fold) Confusion Matrix 
    ## 
    ## (entries are percentual average cell counts across resamples)
    ##  
    ##           Reference
    ## Prediction    N    Y
    ##          N 81.7  7.8
    ##          Y  2.9  7.7
    ##                             
    ##  Accuracy (average) : 0.8935

``` r
plot(rfFit)
```

![](caretDemo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
varImp(rfFit) %>%
  plot()
```

![](caretDemo_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-2.png)

Deployment can be as easy as:

``` r
preds <- 
  bake(myRecipe, newdata = testDF) %>%
  predict(rfFit, .)

# And inspect:
confusionMatrix(preds, testDF$ThirtyDayReadmitFLG)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  N  Y
    ##          N 81  4
    ##          Y  3 11
    ##                                           
    ##                Accuracy : 0.9293          
    ##                  95% CI : (0.8597, 0.9711)
    ##     No Information Rate : 0.8485          
    ##     P-Value [Acc > NIR] : 0.01211         
    ##                                           
    ##                   Kappa : 0.7173          
    ##  Mcnemar's Test P-Value : 1.00000         
    ##                                           
    ##             Sensitivity : 0.9643          
    ##             Specificity : 0.7333          
    ##          Pos Pred Value : 0.9529          
    ##          Neg Pred Value : 0.7857          
    ##              Prevalence : 0.8485          
    ##          Detection Rate : 0.8182          
    ##    Detection Prevalence : 0.8586          
    ##       Balanced Accuracy : 0.8488          
    ##                                           
    ##        'Positive' Class : N               
    ##
