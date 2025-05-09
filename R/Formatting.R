# @file formatting.R
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

#' Convert the plpData in COO format into a sparse R matrix
#'
#' @description
#' Converts the standard plpData to a sparse matrix
#'
#' @details
#' This function converts the covariates `Andromeda` table in COO format into a sparse matrix from
#' the package Matrix
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param cohort                        If specified the plpData is restricted to the rowIds in the cohort (otherwise plpData$labels is used)
#' @param map                           A covariate map (telling us the column number for covariates)
#'
#' @return
#' Returns a list, containing the data as a sparse matrix, the plpData covariateRef
#' and a data.frame named map that tells us what covariate corresponds to each column
#' This object is a list with the following components: \describe{
#' \item{data}{A sparse matrix with the rows corresponding to each person in the plpData and the columns corresponding to the covariates.}
#' \item{covariateRef}{The plpData covariateRef.}
#' \item{map}{A data.frame containing the data column ids and the corresponding covariateId from covariateRef.}
#' }
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' library(dplyr)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=100)
#' # how many covariates are there before we convert to sparse matrix
#' plpData$covariateData$covariates %>% 
#'  dplyr::group_by(.data$covariateId) %>% 
#'  dplyr::summarise(n = n()) %>% 
#'  dplyr::collect() %>% nrow()
#' sparseData <- toSparseM(plpData, cohort=plpData$cohorts)
#' # how many covariates are there after we convert to sparse matrix'
#' sparseData$dataMatrix@Dim[2]
#' }
#' @export
toSparseM <- function(plpData, cohort = NULL, map = NULL) {
  start <- Sys.time()
  ParallelLogger::logInfo(paste0("starting toSparseM"))

  ParallelLogger::logDebug(
    paste0(
      "Max covariateId in original covariates: ",
      plpData$covariateData$covariates %>%
        dplyr::summarise(max = max(.data$covariateId, na.rm = TRUE)) %>%
        dplyr::pull()
    )
  )

  # assign covariateId to colummId specified in map (if null create columnIds)
  # assign rowId to xId
  # return the new covariateData (rowId changes in covariates plus covariates/covariateRef with modified columnIds)
  # return labels with modified rowId if plpData$labels exists

  if (!is.null(plpData$labels)) {
    newcovariateData <- MapIds(
      plpData$covariateData,
      cohort = plpData$labels,
      mapping = map
    )
  } else {
    newcovariateData <- MapIds(
      plpData$covariateData,
      cohort = cohort,
      mapping = map
    )
  }


  ParallelLogger::logDebug(paste0("# covariates in mapped covariateRef: ", nrow(newcovariateData$covariateRef)))

  maxY <- newcovariateData$mapping %>%
    dplyr::summarise(max = max(.data$columnId, na.rm = TRUE)) %>%
    dplyr::pull()
  ParallelLogger::logDebug(paste0("Max newCovariateId in mapping: ", maxY))
  maxX <- newcovariateData$cohort %>%
    dplyr::summarise(max = max(.data$rowId, na.rm = TRUE)) %>%
    dplyr::pull()
  ParallelLogger::logDebug(paste0("Max rowId in new : ", maxX))


  ParallelLogger::logInfo(paste0("toSparseM non temporal used"))

  checkRam(newcovariateData) # estimates size of RAM required and prints it

  data <- Matrix::sparseMatrix(
    i = newcovariateData$covariates %>% dplyr::select("rowId") %>% dplyr::pull(),
    j = newcovariateData$covariates %>% dplyr::select("columnId") %>% dplyr::pull(),
    x = newcovariateData$covariates %>% dplyr::select("covariateValue") %>% dplyr::pull(),
    dims = c(maxX, maxY)
  )

  ParallelLogger::logDebug(paste0("Sparse matrix with dimensionality: ", paste(dim(data), collapse = ",")))

  ParallelLogger::logInfo(paste0("finishing toSparseM"))

  result <- list(
    dataMatrix = data,
    labels = newcovariateData$cohort %>% dplyr::collect(),
    covariateRef = as.data.frame(newcovariateData$covariateRef),
    covariateMap = as.data.frame(newcovariateData$mapping)
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0("toSparseM took ", delta, " ", attr(delta, "units")))
  return(result)
}

#' Map covariate and row Ids so they start from 1
#' @description this functions takes covariate data and a cohort/population and remaps
#' the covariate and row ids, restricts to pop and saves/creates mapping
#' @param covariateData a covariateData object
#' @param cohort        if specified rowIds restricted to the ones in cohort
#' @param mapping       A pre defined mapping to use
#' @returns a new `covariateData` object with remapped covariate and row ids
#' @examples
#' covariateData <- Andromeda::andromeda(
#'   covariates = data.frame(rowId = c(1, 3, 5, 7, 9), 
#'                           covariateId = c(10, 20, 10, 10, 20),
#'                           covariateValue = c(1, 1, 1, 1, 1)),
#'   covariateRef = data.frame(covariateId = c(10, 20), 
#'                               covariateNames = c("covariateA", 
#'                                                  "covariateB"),
#'                               analysisId = c(1, 1)))
#' mappedData <- MapIds(covariateData)
#' # columnId and rowId are now starting from 1 and are consecutive
#' mappedData$covariates
#' @export
MapIds <- function(
    covariateData,
    cohort = NULL,
    mapping = NULL) {
  ParallelLogger::logInfo(paste0("starting to map the columns and rows"))


  # change the rowIds in cohort (if exists)
  if (!is.null(cohort)) {
    rowMap <- data.frame(
      rowId = cohort %>% dplyr::select("rowId")
    )
    rowMap$xId <- 1:nrow(rowMap)
  } else {
    rowMap <- data.frame(
      rowId = covariateData$covariates %>%
        dplyr::distinct(.data$rowId) %>%
        dplyr::pull()
    )
    rowMap$xId <- 1:nrow(rowMap)
  }

  covariateData$rowMap <- rowMap
  on.exit(covariateData$rowMap <- NULL)

  # change the rowIds in covariateData$covariates
  if (is.null(mapping)) {
    mapping <- data.frame(
      covariateId = covariateData$covariates %>%
        dplyr::inner_join(covariateData$rowMap, by = "rowId") %>% # first restrict the covariates to the rowMap$rowId
        dplyr::distinct(.data$covariateId) %>%
        dplyr::arrange(.data$covariateId) %>%
        dplyr::pull()
    )
    mapping$columnId <- 1:nrow(mapping)
  }
  covariateData$mapping <- mapping
  on.exit(covariateData$mapping <- NULL, add = TRUE)

  newCovariateData <- Andromeda::andromeda()
  # change the covariateIds in covariates
  newCovariateData$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$mapping, by = "covariateId")


  # change the covariateIds in covariateRef
  newCovariateData$covariateRef <- covariateData$mapping %>%
    dplyr::inner_join(covariateData$covariateRef, by = "covariateId")

  # change the rowId in covariates
  newCovariateData$rowMap <- rowMap
  newCovariateData$covariates <- newCovariateData$covariates %>%
    dplyr::inner_join(newCovariateData$rowMap, by = "rowId") %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "xId")

  if (!is.null(cohort)) {
    # change the rowId in labels
    newCovariateData$cohort <- cohort %>%
      dplyr::inner_join(rowMap, by = "rowId") %>%
      dplyr::rename(
        originalRowId = "rowId",
        rowId = "xId"
      ) %>%
      dplyr::arrange(.data$rowId) # make sure it is ordered lowest to highest
  }

  newCovariateData$mapping <- mapping
  newCovariateData$analysisRef <- covariateData$analysisRef
  if (!is.null(covariateData$timeRef)) {
    newCovariateData$timeRef <- covariateData$timeRef
  } 
  ParallelLogger::logInfo(paste0("finished MapCovariates"))

  return(newCovariateData)
}

checkRam <- function(covariateData) {
  nrowV <- covariateData$covariates %>%
    dplyr::summarise(size = dplyr::n()) %>%
    dplyr::collect()
  estRamB <- (nrowV$size / 1000000 * 24000984)

  ParallelLogger::logInfo(paste0("plpData size estimated to use ", round(estRamB / 1000000000, 1), "GBs of RAM"))

  return(invisible(TRUE))
}
