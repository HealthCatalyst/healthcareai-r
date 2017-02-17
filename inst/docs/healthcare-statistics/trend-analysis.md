# Trend Analysis via ``findTrends``

## What is this?

In healthcare one often wants to automatically detect trends occuring across many different measures at the same time. Further, one wants to be able to group by various categories and find trends among subsets of the patient population. 

Here one can quickly look across 50+ measures for those that have experienced [notable trends according to Nelson rule 3](https://en.wikipedia.org/wiki/Nelson_rules).

## Why is it helpful?

This ``findTrends`` function allows one to quickly see if any numeric columns in a dataset have been trending downward or upward over six consecutive months, using automatic subgroupings (like on Gender, for example).

## So, how do we do it?

First, get some data organized (via SQL or Excel) that has the following.
- A numeric column (like mortality rate) that could have a trend
- A categorical column (like Gender) that we'll group by to check trends for Females and Males

## Step 1: Create some data or pull it from SQL Server

```r
library(healthcareai)
dates <- c(as.Date("2012-01-01"),as.Date("2012-01-02"),as.Date("2012-02-01"),
      as.Date("2012-03-01"),as.Date("2012-04-01"),as.Date("2012-05-01"),
      as.Date("2012-06-01"),as.Date("2012-06-02"))
y1 <- c(0,1,2,6,8,13,14,16)               # large positive
y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1.5)     # small positive
y3 <- c(1,0,-2,-2,-4,-5,-7,-8)            # big negative
y4 <- c(.5,0,-.5,-.5,-.5,-.5,-.6,0)       # small negative
gender <- c('M','F','F','F','F','F','F','F')
df <- data.frame(dates,y1,y2,y3,y4,gender)
```

## Step 2: Find trends via ``findTrends``

- __Return__: A data frame containing the dimensional attribute (ie gender), the subset the data was grouped by (ie M/F), the measures that had trends (ie, mortality or readmission), and the ending month.
- __Arguments__:
    - __df__: a data frame. This data contains both the (numeric) measure columns and (categorical) columns to group by.
    - __datecol__: a string. Column name for the date or date-time column in your data frame.
    - __coltoaggregate__: a string. Column name for the categorical column we'll group by.

```r
res = findTrends(df = df,
                 dateCol = 'dates',
                 groupbyCol = 'gender')
res
```

## Full example code

```r
library(healthcareai)
dates <- c(as.Date("2012-01-01"),as.Date("2012-01-02"),as.Date("2012-02-01"),
      as.Date("2012-03-01"),as.Date("2012-04-01"),as.Date("2012-05-01"),
      as.Date("2012-06-01"),as.Date("2012-06-02"))
y1 <- c(0,1,2,6,8,13,14,16)               # large positive
y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1.5)     # small positive
y3 <- c(1,0,-2,-2,-4,-5,-7,-8)            # big negative
y4 <- c(.5,0,-.5,-.5,-.5,-.5,-.6,0)       # small negative
gender <- c('M','F','F','F','F','F','F','F')
df <- data.frame(dates,y1,y2,y3,y4,gender)

res = findTrends(df = df,
                 dateCol = 'dates',
                 groupbyCol = 'gender')
res
```
