library(xgboost)
library(healthcareai)
library(dplyr)

# https://rpubs.com/mharris/multiclass_xgboost
# https://github.com/dmlc/xgboost/blob/master/demo/multiclass_classification/train.py


# load data and clean
df <- read.csv(file = '~/RFiles/xgboost demo/derm_data.csv', header = TRUE)
#df <- read.csv(file = '~/Repos/healthcareai-r/derm_data.csv', header = TRUE)

### old way  
# set.seed(42)
# PatientID <- sample(1:dim(df)[1],dim(df[1]))
# 
# row.names(df) <- PatientID
# 
# head(df)
# 
# cols <- names(df)
# df[,cols] <- lapply(df[,cols],as.factor)
# df$x34 <- as.numeric(df$x34)
# df$target <- as.numeric(df$x35)
# df$x35 <- NULL
### end old way

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

### end new way

str(df)

########################################################################
# divide into train and test
## 75% of the sample size
train_index <- sample(1:nrow(df), nrow(df)*0.75)
# Full data set
data_variables <- data.matrix(df[,-35])
data_label <- df[,35]
data_matrix <- xgb.DMatrix(data = data.matrix(df), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index] - 1 # R factors are 1 indexed, XGB is 0 indexed.
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index] - 1 # R factors are 1 indexed, XGB is 0 indexed.
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

test_df <- df[-train_index,]

########################################################################
# set the params and train the cv model
numberOfClasses <- length(unique(data_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses,
                   "max_depth" = 6, 
                   "eta" = 0.1, 
                   "silent" = 0, 
                   "nthread" = 4)
nround    <- 50 # number of XGBoost rounds
# cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
# cv_model <- xgb.cv(params = xgb_params,
#                    data = train_matrix, 
#                    nrounds = nround,
#                    nfold = cv.nfold,
#                    verbose = FALSE,
#                    prediction = TRUE)


# evaluate
# OOF_prediction <- data.frame(cv_model$pred) %>% 
#   mutate(max_prob = max.col(., ties.method = "last"),
#          label = train_label + 1)
# head(OOF_prediction)


########################################################################
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)
# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix, reshape = TRUE)

test_prediction <- test_pred %>% 
  data.frame() %>%
  mutate(predicted_label = max.col(.),
         true_label = test_label + 1) # R factors are 1 indexed, XGB is 0 indexed.

# get target names in order for column naming
targetNamesInOrder <- sort(unique(dfTargets[,35]))
colnames(test_prediction)[1:numberOfClasses] <- targetNamesInOrder

# set up a mapping for the values themselves
from <- 1:numberOfClasses
to <- targetNamesInOrder
map = setNames(to,from)
test_prediction$predicted_label <- map[test_prediction$predicted_label] # note square brackets
test_prediction$true_label <- map[test_prediction$true_label] 
head(test_prediction)

# confusion matrix of test set
caret::confusionMatrix(test_prediction$true_label,
                test_prediction$predicted_label,
                mode = "everything")

all_data <- cbind(row.names(test_data), test_prediction)
colnames(all_data)[1] <- 'PatientID'
# colnames(all_data)[2:(numberOfClasses + 1)] <- class_names

inds <- which(PatientID %in% c(343,302,188,252,91,356))
#print('input data with original labels:')
#print(dfTargets[inds,c(1,2,3,35)])

#print('xgboost output with predictions:')
#head(all_data)

print('xgboost output with predictions and original labels:')
print(cbind(all_data[1:6,], 'target' = dfTargets[inds,35]))

# next order of business: get smart column names into the all_data output.
# then. develop!


