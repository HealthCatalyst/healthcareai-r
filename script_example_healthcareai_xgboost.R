library(healthcareai)

# 1. Load data. Categorical columns should be characters.
csvfile <- system.file("extdata", 
                      "dermatology_multiclass_data.csv", 
                      package = "healthcareai")

 # Replace csvfile with 'path/file'
df <- read.csv(file = csvfile, 
              header = TRUE, 
              stringsAsFactors = FALSE,
              na.strings = c("NULL", "NA", "", "?"))

str(df)

# 2. Develop and save model
set.seed(42)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "multiclass"
p$impute <- TRUE
p$grainCol <- "PatientID"
p$predictedCol <- "target"
p$debug <- FALSE
p$cores <- 1
# xgb_params must be a list with all of these things in it. 
# if you would like to tweak parameters, go for it! 
# Leave objective and eval_metric as they are.
# See more info at \url{https://github.com/dmlc/xgboost/blob/master/doc/parameter.md}
p$xgb_params <- list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "max_depth" = 6, # max depth of each learner
                  "eta" = 0.1, # learning rate
                  "silent" = 0, # verbose output when set to 1
                  "nthread" = 2) # number of processors to use

# Run model
boost <- XGBoostDevelopment$new(p)
boost$run()

# Get output data 
outputDF <- boost$getPredictions()
head(outputDF)
