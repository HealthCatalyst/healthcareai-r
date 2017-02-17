# Find correlations with a specific variable via `CalculateAllCorrelations`

## What is this?

In healthcare (as in other fields) it's often helpful to understand the relationships between the variables in one's dataset. This provides that functionality by finding the [correlations](https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient) for all numeric columns in a particular dataset.

## Why is it helpful?

You can quickly see the relationships present in your data.

## So, how do we do it?

* First, we'll load healthcareai, create a fake dataset on which to work, and look at it:
```r
library(healthcareai)

df <- data.frame(a=c(1,2,3,4,5,6),
b=c(6,5,4,3,2,1),
c=c(3,4,2,1,3,5),
d=c('M','F','F','F','M','F')) #<- is ignored

head(df)
```
* Next, we'll find the correlations between all numeric columns in the dataset represented by `df`.
```r
res <- calculateAllCorrelations(df)
res
```

## Function specs for ``CalculateAllCorrelations``

- __Return__: a data frame of same length as input data frame, but three columns wide.
- __Arguments__:
    - __df__: a data frame. This dataset contains at least two numeric columns. 
    
We use the [Pearson correlation coefficient](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html).

## Full example code

```r
library(healthcareai)

df <- data.frame(a=c(1,2,3,4,5,6),
b=c(6,5,4,3,2,1),
c=c(3,4,2,1,3,5),
d=c('M','F','F','F','M','F')) #<- is ignored

head(df)

res <- calculateAllCorrelations(df)
res
```