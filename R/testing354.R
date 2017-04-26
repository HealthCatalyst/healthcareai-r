# Can delete this line in your work
csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "healthcareai")
# Replace csvfile with '/path/to/yourfile'
df <- read.csv(file = csvfile, header = TRUE, na.strings = c("NULL", "NA", ""))

head(df)

df$PatientID <- NULL
df$InTestWindowFLG <- NULL

set.seed(42)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "classification"
p$impute <- TRUE
p$grainCol <- "PatientEncounterID"
p$predictedCol <- "ThirtyDayReadmitFLG"
p$debug <- FALSE
p$cores <- 1

# Run Lasso
# lasso <- LassoDevelopment$new(p)
# lasso$run()

# set.seed(42) 
# Run Random Forest
rf <- RandomForestDevelopment$new(p)
rf$run()
