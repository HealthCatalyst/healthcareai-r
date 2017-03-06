# Longitudinal Imputation via ``GroupedLOCF``

## What is this?

In healthcare one often works with datasets that have multiple rows for a single person, over time. This is called longitudinal data.

If you want to fill in some of the NULLs in such a dataset, healthcareai lets implement the [last observation carried forward](https://en.wikipedia.org/wiki/Imputation_(statistics)#Single_imputation) technique. In other words, Joe's weight from a year ago (which was his last weight data point) can be pulled forward to Joe's rows corresponding to last week or last month.

## Why is it helpful?

This may help make your models more accurate, or help fill in your data for disparate calculations/visualizations.

## Is any longitudinal dataset ready for healthcareai to work on it?

Nope. You have to first order your data by a PersonID column and then by a date-time column (with time going down the rows).

## So, how do we do it?

* First, we'll load healthcareai and create a fake dataset in R (that you can play with)
```r
library(healthcareai)
df = data.frame(personID=c(1,1,2,2,3,3,3),
                wt=c(.5,NA,NA,NA,.3,.7,NA),
                ht=c(NA,1,3,NA,4,NA,NA),
                date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                       '01/01/2015','01/15/2015','01/30/2015'))

head(df,n=7) # Looking at the raw data
```

* Now let's do the imputation by calling the ``groupedLOCF`` function. LOCF stands for last observation carried forward
```r
dfResult = groupedLOCF(df, 'personID')

head(dfResult,n=7) # Looking at the data that now has fewer NULLs (or NAs)
```


## Function specs for ``groupedLOCF``

- __Return__: a data frame of same shape as input data frame.
- __Arguments__:
    - __df__: a data frame. This data contains NULLs or NAs.
    - __id__: a string. Column name for the PersonID column in your data frame.


## Full example code

```r
library(healthcareai)
df = data.frame(personID=c(1,1,2,2,3,3,3),
                wt=c(.5,NA,NA,NA,.3,.7,NA),
                ht=c(NA,1,3,NA,4,NA,NA),
                date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                       '01/01/2015','01/15/2015','01/30/2015'))

head(df,n=7)

dfResult = groupedLOCF(df, 'personID')

head(dfResult, n = 7)
```


