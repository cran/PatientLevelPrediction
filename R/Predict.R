# @file predict.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' predictPlp
#'
#' @description
#' Predict the risk of the outcome using the input plpModel for the input plpData
#' @details
#' The function applied the trained model on the plpData to make predictions
#' @param plpModel                         An object of type \code{plpModel} - a patient level prediction model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted or a cohort without the outcome known
#' @param timepoint                        The timepoint to predict risk (survival models only)
#' @return A data frame containing the predicted risk values
#' @examples
#' coefficients <- data.frame(
#'   covariateId = c(1002),
#'   coefficient = c(0.05)
#' )
#' model <- createGlmModel(coefficients, intercept = -2.5)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 50)
#' prediction <- predictPlp(model, plpData, plpData$cohorts)
#' # see the predicted risk values
#' head(prediction)
#' @export
predictPlp <- function(plpModel, plpData, population, timepoint) {
  start <- Sys.time()
  if (is.null(plpModel)) {
    stop("No model input")
  }
  if (is.null(population)) {
    stop("No population input")
  }
  if (is.null(plpData)) {
    stop("No plpData input")
  }


  # do feature engineering/selection
  if (!is.null(plpModel$preprocessing$featureEngineering)) {
    plpData <- do.call(
      applyFeatureEngineering,
      list(
        plpData = plpData,
        settings = plpModel$preprocessing$featureEngineering
      )
    )
    featureEngineering <- TRUE
  } else {
    featureEngineering <- FALSE
  }

  ParallelLogger::logTrace("did FE")

  if (!is.null(plpModel$preprocessing$tidyCovariates)) {
    # do preprocessing
    plpData$covariateData <- do.call(
      applyTidyCovariateData,
      list(
        covariateData = plpData$covariateData,
        preprocessSettings = plpModel$preprocessing$tidyCovariates
      )
    )
    tidyCovariates <- TRUE
  } else {
    tidyCovariates <- FALSE
  }

  ParallelLogger::logTrace("did tidy")

  # add timepoint if not missing to population attribute
  if (!missing(timepoint)) {
    attr(population, "timepoint") <- timepoint
  } else {
    timepoint <- attr(population, "metaData")$populationSettings$riskWindowEnd
  }

  # apply prediction function
  prediction <- do.call(
    eval(parse(text = attr(plpModel, "predictionFunction"))),
    list(
      plpModel = plpModel,
      data = plpData,
      cohort = population
    )
  )

  if (!is.null(attr(prediction, "metaData"))) {
    metaData <- attr(prediction, "metaData")
  } else {
    metaData <- list()
  }

  # add metaData
  metaData$modelType <- attr(plpModel, "modelType") # "binary",
  metaData$targetId <- attr(population, "metaData")$targetId
  metaData$outcomeId <- attr(population, "metaData")$outcomeId
  metaData$timepoint <- timepoint

  # added information about running preprocessing/FE
  metaData$tidyCovariates <- tidyCovariates
  metaData$featureEngineering <- featureEngineering

  attr(prediction, "metaData") <- metaData
  delta <- Sys.time() - start
  ParallelLogger::logInfo(
    "Prediction done in: ",
    signif(delta, 3), " ", attr(delta, "units")
  )
  return(prediction)
}



applyFeatureEngineering <- function(
    plpData,
    settings) {
  # if a single setting make it into a list
  if (!is.null(settings$funct)) {
    settings <- list(settings)
  }

  # add code for implementing the feature engineering
  for (set in settings) {
    set$settings$trainData <- plpData
    plpData <- do.call(eval(parse(text = set$funct)), set$settings)
  }

  # dont do anything for now
  return(plpData)
}


# NEED TO UPDATE....
# fucntion for implementing the pre-processing (normalisation and redundant features removal)
applyTidyCovariateData <- function(
    covariateData,
    preprocessSettings) {
  if (!FeatureExtraction::isCovariateData(covariateData)) {
    stop("Data not of class CovariateData")
  }

  newCovariateData <- Andromeda::andromeda(
    covariateRef = covariateData$covariateRef,
    analysisRef = covariateData$analysisRef
  )
  if (!is.null(covariateData$timeRef)) {
    newCovariateData$timeRef <- covariateData$timeRef
  }

  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds

  # --- added for speed
  deleteCovariateIds <- c(deleteRedundantCovariateIds, deletedInfrequentCovariateIds)
  temp <- covariateData$covariateRef %>% dplyr::collect()
  allCovariateIds <- temp$covariateId
  covariateData$includeCovariates <- data.frame(covariateId = allCovariateIds[!allCovariateIds %in% deleteCovariateIds])
  if (inherits(covariateData, "RSQLiteConnection")) {
    Andromeda::createIndex(covariateData$includeCovariates, c("covariateId"),
      indexName = "includeCovariates_covariateId"
    )
  }
  on.exit(covariateData$includeCovariates <- NULL, add = TRUE)
  # ---

  ParallelLogger::logInfo("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()

  if (!is.null(maxs)) {
    if ("bins" %in% colnames(maxs)) {
      covariateData$maxes <- dplyr::as_tibble(maxs) %>%
        dplyr::rename(covariateId = "bins") %>%
        dplyr::rename(maxValue = "maxs")
    } else {
      covariateData$maxes <- maxs
    }
    on.exit(covariateData$maxes <- NULL, add = TRUE)

    if (inherits(covariateData, "RSQLiteConnection")) {
      # --- added for speed
      Andromeda::createIndex(covariateData$maxes, c("covariateId"),
        indexName = "maxes_covariateId"
      )
    } # ---

    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$includeCovariates, by = "covariateId") %>% # added as join
      dplyr::inner_join(covariateData$maxes, by = "covariateId") %>%
      dplyr::mutate(value = 1.0 * .data$covariateValue / .data$maxValue) %>%
      dplyr::select(-"covariateValue") %>%
      dplyr::rename(covariateValue = "value")
  } else {
    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$includeCovariates, by = "covariateId")
  }

  # reduce covariateRef
  newCovariateData$covariateRef <- covariateData$covariateRef %>%
    dplyr::inner_join(covariateData$includeCovariates, by = "covariateId")


  if (inherits(newCovariateData, "RSQLiteConnection")) {
    # adding index for restrict to pop
    Andromeda::createIndex(
      newCovariateData$covariates,
      c("rowId"),
      indexName = "ncovariates_rowId"
    )
  }
  class(newCovariateData) <- "CovariateData"

  delta <- Sys.time() - start
  writeLines(paste("Removing infrequent and redundant covariates covariates and normalizing took", signif(delta, 3), attr(delta, "units")))

  # return processed data
  return(newCovariateData)
}
