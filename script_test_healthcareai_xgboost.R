library(xgboost)
library(healthcareai)
library(dplyr)

# https://rpubs.com/mharris/multiclass_xgboost
# https://github.com/dmlc/xgboost/blob/master/demo/multiclass_classification/train.py


# load data and clean
df <- read.csv(file = '~/RFiles/xgboost demo/derm_data.csv', header = TRUE)
# df <- read.csv(file = '~/Repos/healthcareai-r/derm_data.csv', header = TRUE)

### new way
set.seed(42)
PatientID <- sample(1:dim(df)[1],dim(df[1]))

row.names(df) <- PatientID

df$target <- df$x35
df$x35 <- NULL
df$target[df$target==1] <- 'one'
df$target[df$target==2] <- 'two'
df$target[df$target==3] <- 'three'
df$target[df$target==4] <- 'four'
df$target[df$target==5] <- 'five'
df$target[df$target==6] <- 'six'
head(df)
dfTargets <- df
cols <- names(df)
df[,cols] <- lapply(df[,cols],as.factor)
df$x34 <- as.numeric(df$x34)
df$target <- as.numeric(df$target)

# set up data frame
set.seed(42)

p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "multiclass"
p$impute <- TRUE
p$grainCol <- ""
p$predictedCol <- "target"
p$debug <- FALSE
p$cores <- 4

boost <- XGBoostDevelopment$new(p)
boost$run()

