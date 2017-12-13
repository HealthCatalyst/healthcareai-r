# To add a model or customize hyperparameters to tune over
# e.g. caret's implementation of randomForest only tunes mtry, so add ntree
# https://topepo.github.io/caret/using-your-own-model-in-train.html

# Setup
library(tidyverse)
library(caret)
csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "healthcareai")
df <- read.csv(file = csvfile, header = TRUE, na.strings = c("NULL", "NA", ""))
df <- na.omit(df)

# Load caret's list of model implementation details:
load("/Library/Frameworks/R.framework/Versions/3.4/Resources/library/caret/models/models.RData")
# Pull randomForest
my_rf <- models$rf

# To add other hyperparameters to tune, need to adjust parameters
# and grid if the method might have to construct a grid
my_rf$parameters <- data.frame(parameter = c("ntree", "mtry"),
                               class = c("numeric", "numeric"),
                               label = c("number trees", "variables tried"))

# Set up CV
grid <- expand.grid(mtry = c(1, 4), ntree = c(50, 500))
cvDetails <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                          summaryFunction = twoClassSummary, search = "grid")
rf_train <-
  train(x = dplyr::select(df, -PatientEncounterID, -ThirtyDayReadmitFLG),
        y = df$ThirtyDayReadmitFLG,
        metric = "ROC",
        method = my_rf,
        tuneGrid = grid,
        trControl = cvDetails)
rf_train
plot(rf_train)


# Potentially useful:
# my_rf$grid: function(x, y, len = NULL, search = "grid") {
#   if(search == "grid") {
#     out <- data.frame(mtry = caret::var_seq(p = ncol(x),
#                                             classification = is.factor(y),
#                                             len = len))
#   } else {
#     out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
#   }
#   out
# }

# caret::var_seq: function (p, classification = FALSE, len = 3)
# {
#     if (len == 1) {
#         tuneSeq <- if (classification)
#             max(floor(p/3), 1)
#         else floor(sqrt(p))
#     }
#     else {
#         if (p <= len) {
#             tuneSeq <- floor(seq(2, to = p, length = p))
#         }
#         else {
#             if (p < 500)
#                 tuneSeq <- floor(seq(2, to = p, length = len))
#             else tuneSeq <- floor(2^seq(1, to = log(p, base = 2),
#                 length = len))
#         }
#     }
#     if (any(table(tuneSeq) > 1)) {
#         tuneSeq <- unique(tuneSeq)
#         cat("note: only", length(tuneSeq), "unique complexity parameters in default grid.",
#             "Truncating the grid to", length(tuneSeq), ".\n\n")
#     }
#     tuneSeq
# }
