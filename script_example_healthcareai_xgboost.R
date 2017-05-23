library(healthcareai)

# 1. Load data. Categorical colum
csvfile <- system.file("extdata", 
                      "dermatology_multiclass_data.csv", 
                      package = "healthcareai")

 # Replace csvfile with 'path/file'
df <- read.csv(file = csvfile, 
              header = TRUE, 
              stringsAsFactors = FALSE,
              na.strings = c("NULL", "NA", "", "?"))

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
head(outputDF)
