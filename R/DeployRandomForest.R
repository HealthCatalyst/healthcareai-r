# Import the common functions.
source('R/common.R')
source('R/DeploySupervisedModel.R')


DeployRandomForest <- R6Class("DeployRandomForest",

  #Inheritance
  inherit = DeploySupervisedModel,

  #Private members
  private = list(

    dfTestTemp = NULL,
    dfTrainTemp = NULL,

    coefficients = NULL,
    multiply_res = NULL,
    ordered.factors = NULL

  ),

  #Public members
  public = list(

    #Constructor
    #p: new DeploySupervisedModelParameters class object, i.e. p = DeploySupervisedModelParameters$new()
    initialize = function (p) {
      super$initialize(p)
    },

    connectDataSource = function () {
      odbcCloseAll()
      # Convert the connection string into a real connection object.
      self$params$sqlConn <- odbcDriverConnect(self$params$sqlConn)
    },

    closeDataSource = function () {
      odbcCloseAll()
    },

    fitGeneralizedLinearModel = function () {

      if (self$params$type == 'classification') {

        private$fit.logit = glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          family = binomial(link = "logit"),
          metric = "ROC",
          control = list(maxit = 10000),
          trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary))

      } else if (self$params$type == 'regression') {

        private$fit.logit = glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          metric = "RMSE",
          control = list(maxit = 10000))
      }
    },

    fitRandomForest = function () {

      # Set proper mtry (either based on recc default or specified)
      if (nchar(self$params$rfmtry) == 0 && self$params$type == 'classification') {
        rfmtry.temp <- floor(sqrt(ncol(private$dfTrain)))
      } else if (nchar(self$params$rfmtry) == 0 && self$params$type == 'regression') {
        rfmtry.temp <- max(floor(ncol(private$dfTrain)/3), 1)
      } else {
        rfmtry.temp <- self$params$rfmtry
      }

      if (self$params$type == 'classification') {

        private$fit = ranger(
          as.formula(paste(selfself$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          probability = TRUE,
          num.trees = self$params$trees,
          write.forest = TRUE,
          mtry = rfmtry.temp)

      } else if (self$params$type == 'regression') {

        private$fit = ranger(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          num.trees = self$params$trees,
          write.forest = TRUE,
          mtry = rfmtry.temp)
      }

    },

    #Override: Build fit object
    buildFitObject= function () {

      # Get fit object by random forest
      self$fitRandomForest()

    },

    buildDeployModel = function () {

      if (isTRUE(self$params$debug)) {
        print('Training data set immediately before training')
        print(str(private$dfTrain))
      }

      # Start default logit (for var importance)
      self$fitGeneralizedLinearModel()

      # Build fit object
      self$buildFitObject()

      print('Details for proability model:')
      print(private$fit)

    },

    saveModel = function () {

      # Save models if specified
      if (isTRUE(!self$params$useSavedModel)) {
        save(private$fit.logit, file = "rmodel_var_import.rda")
        save(private$fit, file = "rmodel_probability.rda")
      }

      # This isn't needed if formula interface is used in randomForest
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        print('Test set before being used in predict(), after removing y')
        print(str(private$dfTest))
      }

    },

    performPrediction = function () {

      if (self$params$type == 'classification') {
        #  these are probabilities
        predictedVALStemp = predict(private$fit, data = private$dfTest)
        private$predictedVALS <- predictedVALStemp$predictions[,2]

        print('Probability predictions are based on random forest')

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in prob prediction: ', nrow(private$predictedVALS)))
          print('First 10 raw classification probability predictions')
          print(round(private$predictedVALS[1:10],2))
        }

      } else if (self$type == 'regression') {
        # this is in-kind prediction

        predictedVALStemp = predict(fit, data = self$dfTest)
        private$predictedVALS <- predictedVALStemp$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ', length(private$predictedVALS)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictedVALS[1:10],2))
        }

      }

    },

    calculateCoeffcients = function () {

      # Do semi-manual calc to rank cols by order of importance
      coefftemp <- private$fit.logit$coefficients

      if (isTRUE(self$params$debug)) {
        print('Coefficients for the default logit (for ranking var import)')
        print(coefftemp)
      }

      private$coefficients <- coefftemp[2:length(coefftemp)] # drop intercept

    },

    calculateMultiplyRes = function () {

      # Apply multiplication of coeff across each row of test set
      # Remove y (label) so we do multiplication only on X (features)
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        print('Test set after removing predicted column')
        print(str(private$dfTest))
      }

      private$multiply_res <- sweep(private$dfTestRAW, 2, private$coefficients, `*`)

      if (isTRUE(self$params$debug)) {
        print('Data frame after multiplying raw vals by coeffs')
        print(private$multiply_res[1:10,])
      }

    },

    calculateOrderedFactors = function () {

      # Calculate ordered factors of importance for each row's prediction
      private$ordered.factors = t(sapply
                          (1:nrow(private$multiply_res),
                          function(i)
                            colnames(private$multiply_res[order(private$multiply_res[i,],
                                                        decreasing = TRUE)])))

      if (isTRUE(self$params$debug)) {
        print('Data frame after getting column importance ordered')
        print(private$ordered.factors[1:10,])
      }

    },

    saveDataIntoSAMDb = function () {

      dtstamp = as.POSIXlt(Sys.time(), "GMT")

      # Combine grain.col, prediction, and time to be put back into SAM table
      outdf <- data.frame(0,                           # BindingID
                          'R',                         # BindingNM
                          dtstamp,                     # LastLoadDTS
                          private$grainTest,           # GrainID
                          private$predictedVALS,       # PredictedProbab
                          private$ordered.factors[,1:3]) # Top 3 Factors

      prediectedResultsName = ""
      if (self$type == 'classification') {
        prediectedResultsName = "PredictedProbNBR"
      } else if (self$type == 'regression') {
        prediectedResultsName = "PredictedValueNBR"
      }
      colnames(outdf) <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol,
        prediectedResultsName,
        "Factor1TXT",
        "Factor2TXT",
        "Factor3TXT"
      )

      if (isTRUE(self$params$debug)) {
        print('Dataframe going to SQL Server:')
        print(str(outdf))
      }

      # Save df to table in SAM database
      out = sqlSave(channel = self$params$sqlConn,
                    dat = outdf,
                    tablename = self$params$destSchemaTable,
                    append = T,
                    rownames = F,
                    colnames = F,
                    safer = T,
                    nastring = NULL,
                    verbose = self$params$debug)

      # Print success if insert was successful
      if (out == 1) {
        print('SQL Server insert was successful')
      }

    },

    #Override: deploy the model
    deploy = function () {

      # Connect to sql via odbc driver
      self$connectDataSource()

      if (isTRUE(self$params$useSavedModel)) {
        load("rmodel_var_import.rda")  # Produces fit.logit object
        load("rmodel_probability.rda") # Produces fit object (for probability)
      } else {

        private$registerClustersOnCores()

        # build deploy model
        self$buildDeployModel()
      }

      # Save model
      self$saveModel()

      # Predict
      self$performPrediction()

      # Calculate Coeffcients
      self$calculateCoeffcients()

      # Calculate MultiplyRes
      self$calculateMultiplyRes()

      # Calculate Ordered Factors
      self$calculateOrderedFactors()

      # Save data into SAM db
      self$saveDataIntoSAMDb()

      # Clean up.
      private$stopClustersOnCores()
      self$closeDataSource()
    }
  )

)
