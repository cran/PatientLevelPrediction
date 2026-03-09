## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(PatientLevelPrediction)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
# settings <- list(
#   seed = 12,
#   modelName = "Special classifier",
#   trainRFunction = 'madeupFit', # this will be called to train the made up model
#   predictRFunction = 'madeupPrediction', # this will be called to make predictions
#   varImpRFunction = 'madeupVarImp' # this will be called to get variable importance
# )

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# setMadeUp <- function(a = c(1, 4, 10), b = 2, seed = NULL) {
#   # add input checks here...
# 
#   param <- list(
#     a = a,
#     b = b
#     )
# 
#   settings <- list(
#     modelName = "Made Up",
#     requiresDenseMatrix = TRUE,
#     seed = seed,
#     trainRFunction = 'madeupFit', # this will be called to train the made up model
#     predictRFunction = 'madeupPrediction', # this will be called to make predictions
#     varImpRFunction = 'madeupVarImp', # this will be called to get variable importance
#     saveToJson = TRUE,
#     saveType = "file"
#   )
# 
#   # now create list of all combinations:
#   result <- list(
#     fitFunction = "fitBinaryClassifier", # this will be called to train the made up model
#     param = param,
#     settings = settings
#   )
#   class(result) <- "modelSettings"
# 
#   return(result)
# }

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# fitMadeUp <- function(
#     dataMatrix,
#     labels,
#     hyperParameters,
#     settings
#     ) {
#   # set the seed for reproducibility
#   set.seed(settings$seed)
# 
#   # add code here to call a function to train the classifier using the data
#   # for the specified hyperparameters that are a named list in hyperParameters
#   model <- madeUpModel(
#     X = dataMatrix,
#     Y = labels$outcomeCount,
#     hyperparameter1 = hyperParameters$hyperparameter1,
#     hyperparameter2 = hyperParameters$hyperparameter2
#     )
# 
#   return(model)
# }

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# setMadeUp <- function(a = c(1, 4, 6), b = 2, seed = NULL) {
#   # add input checks here...
# 
#   if (is.null(seed)) {
#     seed <- sample(100000, 1)
#   }
# 
#   param <- list(
#       a = a,
#       b = b
#     )
# 
#   settings <- list(
#     modelName = "Made Up",
#     requiresDenseMatrix = TRUE,
#     seed = seed,
#     trainRFunction = 'fitMadeUp',
#     predictRFunction = 'predictMadeUp',
#     varImpRFunction = 'varImpMadeUp',
#     saveToJson = TRUE,
#     saveType = "file"
#   )
# 
#   # now create list of all combinations:
#   result <- list(
#     fitFunction = "fitBinaryClassifier", # this is an existing wrapper fit function
#     param = param,
#     settings = settings
#   )
#   class(result) <- "modelSettings"
# 
#   return(result)
# }

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# fitMadeUp <- function(
#     dataMatrix,
#     labels,
#     hyperParameters,
#     settings
#     ) {
#   # set the seed for reproducibility
#   set.seed(settings$seed)
# 
#   # add code here to call a function to train the classifier using the data
#   # for the specified hyperparameters that are a named list in hyperParameters
#   model <- madeUpModel(
#     X = dataMatrix,
#     Y = labels$outcomeCount,
#     hyperparameter1 = hyperParameters$hyperparameter1,
#     hyperparameter2 = hyperParameters$hyperparameter2
#     )
# 
#   return(model)
# }

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# predictMadeUp <- function(plpModel, data, cohort) {
#   if (class(data) == "plpData") {
#     # convert
#     matrixObjects <- toSparseM(
#       plpData = data,
#       cohort = cohort,
#       map = plpModel$covariateImportance %>%
#         dplyr::select("columnId", "covariateId")
#     )
# 
#     newData <- matrixObjects$dataMatrix
#     cohort <- matrixObjects$labels
#   } else {
#     newData <- data
#   }
# 
#   if (class(plpModel) == "plpModel") {
#     model <- plpModel$model
#   } else {
#     model <- plpModel
#   }
# 
#   cohort$value <- stats::predict(model, newData)
# 
#   # fix the rowIds to be the old ones
#   # now use the originalRowId and remove the matrix rowId
#   cohort <- cohort %>%
#     dplyr::select(-"rowId") %>%
#     dplyr::rename(rowId = "originalRowId")
# 
#   attr(cohort, "metaData") <- list(modelType = attr(plpModel, "modelType"))
#   return(cohort)
# }

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# varImpMadeUp <- function(
#     model,
#     covariateMap
#     ) {
# 
#   variableImportance <- tryCatch(
#       {
#         varImp <- reticulate::py_to_r(model$feature_importances_)
#         varImp[is.na(varImp)] <- 0
# 
#         covariateMap$covariateValue <- unlist(varImp)
#         covariateMap$included <- 1
#       },
#       error = function(e) {
#         ParallelLogger::logInfo(e)
#         return(NULL)
#       }
#     )
# 
#   return(variableImportance)
# }

## ----tidy=TRUE,eval=TRUE------------------------------------------------------
citation("PatientLevelPrediction")

