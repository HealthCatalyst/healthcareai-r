# Find correlations with a specific variable via `calculateTargetedCorrelations`

## What is this?

When trying to understand the factors driving a particular processes, it can be helpful to view the [correlations](https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient) between several variables and your variable of interest. This function lets one quickly check the [correlations](https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient) between all numeric fields in a dataset and a specified column.

## Why is it helpful?

You can quickly see how well other variables explain your variable of interest.

## So, how do we do it?

* First, we'll load healthcareai, create a fake dataset on which to work, and look at it:

```r
library(healthcareai)

df <- data.frame(a=c(1,2,3,4,5,6),
b=c(6,5,4,3,2,1),
c=c(3,4,2,1,3,5),
d=c('M','F','F','F','M','F')) #<- categorical coulmns are ignored

head(df)
```

* Next, we'll find the correlations between `'c'` and the other numeric columns in the data represented by `df`.

```r
res <- calculateTargetedCorrelations(df,'c')
res
```

## Function specs for ``calculateTargetedCorrelations``

- __Return__: a data frame of same length as input data frame, but three columns wide (column name, correlation, p-value).
- __Arguments__:
    - __df__: a data frame. This dataset contains at least two numeric columns.
    - __target.col__: a string. Column name of the variable of interest. Correlations of all other numeric columns are calculated against this column. 
    
We use the [Pearson correlation coefficient](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html).
For details on the p-value calculation, see [here](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html)

## Full example code

```r
library(healthcareai)

df <- data.frame(a=c(1,2,3,4,5,6),
b=c(6,5,4,3,2,1),
c=c(3,4,2,1,3,5),
d=c('M','F','F','F','M','F')) #<- categorical coulmns are ignored

head(df)

res <- calculateTargetedCorrelations(df,'c')
res
```