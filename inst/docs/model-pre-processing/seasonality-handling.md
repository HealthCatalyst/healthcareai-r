# Seasonality handling via ``convertDateTimeColToDummies``

## What is this?

In healthcare date-time stamp columns are common, but unfortunately machine learning algorithms do not handle them well.

## Why is it helpful?

One has to do some simple feature engineering in order to take advantage of potential seasonality effects in a dataset. This works simply by transforming a date-time column into multiple columns that represent MonthOfYear, WeekOfYear, DayOfMonth, etc.

## So, how do we do it?

* First, we'll load healthcareai, create a fake dataset on which to work, and look at it:
```r
dtCol = c("2001-06-09 12:45:05","2002-01-29 09:30:05","2002-02-02 07:36:50",
          "2002-03-04 16:45:01","2002-11-13 20:00:10","2003-01-29 07:31:43",
          "2003-07-07 17:30:02","2003-09-28 01:03:20")
y1 <- c(.5,1,3,6,8,13,14,1)
y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1)
df <- data.frame(dtCol,y1,y2)

head(df)
```
* Next, we'll create the extra date and time colums by calling the function and then we'll look at the transformed dataset
```r
df <- convertDateTimeColToDummies(df, 'dtCol')
head(df)
```

## Function specs for `convertDateTimeColToDummies`

- __Return__: a data frame of same length, but greater width compared to the input data frame.
- __Arguments__:
    - __df__: a data frame. This data frame contains a date-time column.
    - __date.time.col__: a string. Column name for the date-time column in your data frame that you want to split into multiple date and time columns. Works best in ISO 8601 format (ie, datetime or datetime2 in T-SQL).
    - __depth__: a string, defaults to `'h'`. Indicates how many columns should be added to data frame. `'d'`, `'h'`, `'m'`, `'s'` expands to depth of day, hour, minute, and second, respectively. 
    - __return.dt.col__: boolean, defaults to `FALSE`. Indicates whether to return original date-time column with modified data frame.

## Full example code

```r
dtCol = c("2001-06-09 12:45:05","2002-01-29 09:30:05","2002-02-02 07:36:50",
          "2002-03-04 16:45:01","2002-11-13 20:00:10","2003-01-29 07:31:43",
          "2003-07-07 17:30:02","2003-09-28 01:03:20")
y1 <- c(.5,1,3,6,8,13,14,1)
y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1)
df <- data.frame(dtCol,y1,y2)

df <- convertDateTimeColToDummies(df, 'dtCol')
head(df)
```