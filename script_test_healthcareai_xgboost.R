library(xgboost)
library(healthcareai)
library(dplyr)
library(R6)
library(caret)
source('~/healthcareai-r/R/xgboost-development.R')
source('~/healthcareai-r/R/supervised-model-development.R')


# https://rpubs.com/mharris/multiclass_xgboost
# https://github.com/dmlc/xgboost/blob/master/demo/multiclass_classification/train.py


# load data and clean
df <- read.csv(file = '~/RFiles/xgboost demo/derm_data.csv', header = TRUE)
# df <- read.csv(file = '~/Repos/healthcareai-r/derm_data.csv', header = TRUE)

### new way
set.seed(42)
PatientID <- sample(1:dim(df)[1],dim(df[1]))

row.names(df) <- PatientID
df$PatientID <- PatientID
df$target <- df$x35
df$x35 <- NULL
df$target[df$target==1] <- 'one'
df$target[df$target==2] <- 'two'
df$target[df$target==3] <- 'three'
df$target[df$target==4] <- 'four'
df$target[df$target==5] <- 'five'
df$target[df$target==6] <- 'six'

dfTargets <- df
cols <- names(df)
df[,cols] <- lapply(df[,cols],as.factor)
df$x34 <- as.numeric(df$x34)
df$target <- dfTargets['target']


str(df)

# set up data frame
set.seed(42)

p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "multiclass"
p$impute <- TRUE
p$grainCol <- "PatientID"
p$predictedCol <- "target"
p$debug <- FALSE
p$cores <- 1
p$xgb_params <- list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = NA,
                  "max_depth" = 7, 
                  "eta" = 0.2, 
                  "silent" = 0, 
                  "nthread" = 2)

boost <- XGBoostDevelopment$new(p)
boost$run()

outputDF <- boost$getPredictions()

