# @file ViewShinyPlp.R
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

#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' @return Opens a shiny app for interactively viewing the results
#'
#' @examplesIf rlang::is_interactive() && rlang::is_installed("Eunomia") && rlang::is_installed("OhdsiShinyAppBuilder") && rlang::is_installed("curl") && curl::has_internet()  
#' \donttest{ \dontshow{ # takes too long }
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' Eunomia::createCohorts(connectionDetails)
#' databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails,
#'                                           cdmDatabaseSchema = "main",
#'                                           cdmDatabaseName = "Eunomia",
#'                                           cdmDatabaseId = "1",
#'                                           targetId = 1,
#'                                           outcomeIds = 3)
#' modelDesign <- createModelDesign(targetId = 1, 
#'                                  outcomeId = 3, 
#'                                  modelSettings = setLassoLogisticRegression())
#' saveLoc <- file.path(tempdir(), "viewMultiplePlp", "development")
#' runMultiplePlp(databaseDetails = databaseDetails, modelDesignList = list(modelDesign),
#'                saveDirectory = saveLoc)
#' # view result files
#' dir(saveLoc, recursive = TRUE)
#' # open shiny app
#' viewMultiplePlp(analysesLocation = saveLoc)
#' # clean up, shiny app can't be opened after the following has been run
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
viewMultiplePlp <- function(analysesLocation) {
  if (!file.exists(file.path(analysesLocation, "sqlite", "databaseFile.sqlite"))) {
    stop("No database found")
  }

  connectionDetailSettings <- list(
    dbms = "sqlite",
    server = file.path(analysesLocation, "sqlite", "databaseFile.sqlite")
  )

  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings,
    schema = "main",
    tablePrefix = "",
    dbms = "sqlite",
    server = file.path(analysesLocation, "sqlite", "databaseFile.sqlite"),
    user = NULL,
    password = NULL,
    port = NULL
  )

  viewPlps(databaseSettings)
}

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @param diagnosePlp  The output of diagnosePlp()
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @examplesIf rlang::is_interactive() && rlang::is_installed("OhdsiShinyAppBuilder") 
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n= 1000)
#' saveLoc <- file.path(tempdir(), "viewPlp", "development")
#' results <- runPlp(plpData, saveDirectory = saveLoc)
#' # view result files
#' dir(saveLoc, recursive = TRUE)
#' # open shiny app
#' viewPlp(results)
#' # clean up, shiny app can't be opened after the following has been run
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
viewPlp <- function(runPlp, validatePlp = NULL, diagnosePlp = NULL) {
  server <- insertRunPlpToSqlite(
    runPlp = runPlp,
    externalValidatePlp = validatePlp,
    diagnosePlp = diagnosePlp
  )

  connectionDetailSettings <- list(
    dbms = "sqlite",
    server = server
  )

  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings,
    schema = "main",
    tablePrefix = "",
    dbms = "sqlite",
    server = server,
    user = NULL,
    password = NULL,
    port = NULL
  )

  viewPlps(databaseSettings)
}


#' open a local shiny app for viewing the result of a PLP analyses from a database
#'
#' @details
#' Opens a shiny app for viewing the results of the models from a database
#'
#' @param mySchema  Database result schema containing the result tables
#' @param myServer server with the result database
#' @param myUser Username for the connection to the result database
#' @param myPassword Password for the connection to the result database
#' @param myDbms database management system for the result database
#' @param myPort Port for the connection to the result database
#' @param myTableAppend A string appended to the results tables (optional)
#' @return Opens a shiny app for interactively viewing the results
#'
#' @examplesIf rlang::is_interactive() && rlang::is_installed("Eunomia") && rlang::is_installed("OhdsiShinyAppBuilder") && rlang::is_installed("curl") && curl::has_internet()  
#' \donttest{  \dontshow{ # takes too long }
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' Eunomia::createCohorts(connectionDetails)
#' databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails,
#'                                           cdmDatabaseSchema = "main",
#'                                           cdmDatabaseName = "Eunomia",
#'                                           cdmDatabaseId = "1",
#'                                           targetId = 1,
#'                                           outcomeIds = 3)
#' modelDesign <- createModelDesign(targetId = 1,
#'                                  outcomeId = 3,
#'                                  modelSettings = setLassoLogisticRegression())
#' saveLoc <- file.path(tempdir(), "viewDatabaseResultPlp", "developement")
#' runMultiplePlp(databaseDetails = databaseDetails, modelDesignList = list(modelDesign),
#'                saveDirectory = saveLoc)
#' # view result files
#' dir(saveLoc, recursive = TRUE)
#' viewDatabaseResultPlp(myDbms = "sqlite", 
#'                       mySchema = "main", 
#'                       myServer = file.path(saveLoc, "sqlite", "databaseFile.sqlite"),
#'                       myUser = NULL,
#'                       myPassword = NULL,
#'                       myTableAppend = "")
#' # clean up, shiny app can't be opened after the following has been run
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
viewDatabaseResultPlp <- function(
    mySchema,
    myServer,
    myUser,
    myPassword,
    myDbms,
    myPort = NULL,
    myTableAppend) {
  connectionDetailSettings <- list(
    dbms = myDbms,
    server = myServer,
    user = myUser,
    password = myPassword,
    port = myPort
  )

  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings,
    schema = mySchema,
    tablePrefix = myTableAppend,
    dbms = myDbms,
    server = myServer,
    user = myUser,
    password = myPassword,
    port = myPort
  )

  viewPlps(databaseSettings)
}



# code for multiple and single together
# one shiny app

viewPlps <- function(databaseSettings) {
  rlang::check_installed("OhdsiShinyAppBuilder")
  rlang::check_installed("ResultModelManager")
  connectionDetails <- do.call(
    DatabaseConnector::createConnectionDetails,
    databaseSettings$connectionDetailSettings
  )
  connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  databaseSettings$connectionDetailSettings <- NULL

  config <- ParallelLogger::loadSettingsFromJson(
        fileName = system.file(
          "shinyConfigUpdate.json",
          package = "PatientLevelPrediction"
        )
      )
      databaseSettings$plpTablePrefix <- databaseSettings$tablePrefix
      databaseSettings$cgTablePrefix <- databaseSettings$tablePrefix
      databaseSettings$databaseTable <- "database_meta_data"
      databaseSettings$databaseTablePrefix <- databaseSettings$tablePrefix
      OhdsiShinyAppBuilder::viewShiny(
        config = config,
        connection = connection,
        resultDatabaseSettings = databaseSettings
      )
}
