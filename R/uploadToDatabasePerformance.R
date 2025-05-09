# @file uploadToDatabasePerformance.R
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
insertPerformanceInDatabase <- function(
    performanceEvaluation = NULL,
    covariateSummary = NULL,
    attrition = NULL,
    executionDateTime,
    plpVersion,
    conn,
    databaseSchemaSettings,
    modelDesignId,
    developmentDatabaseId,
    validationDatabaseId,
    validationTarId,
    validationPopulationId,
    validationPlpDataId,
    validationTargetId,
    validationOutcomeId,
    modelDevelopment # added
    ) {
  ParallelLogger::logInfo(paste0("Inserting performance..."))

  # add the results
  performanceId <- addPerformance(
    conn = conn,
    resultSchema = databaseSchemaSettings$resultSchema,
    targetDialect = databaseSchemaSettings$targetDialect,
    modelDesignId = modelDesignId,
    developmentDatabaseId = developmentDatabaseId,
    validationDatabaseId = validationDatabaseId,
    validationTargetId = validationTargetId,
    validationOutcomeId = validationOutcomeId,
    validationTarId = validationTarId,
    validationPlpDataId = validationPlpDataId,
    validationPopulationId = validationPopulationId,
    modelDevelopment = modelDevelopment,
    executionDateTime = executionDateTime,
    plpVersion = plpVersion,
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0("performanceId: ", performanceId))

  # add attrition
  if (!is.null(attrition)) {
    addAttrition(
      conn = conn,
      resultSchema = databaseSchemaSettings$resultSchema,
      targetDialect = databaseSchemaSettings$targetDialect,
      performanceId = performanceId,
      attrition = attrition,
      overWriteIfExists = TRUE,
      tablePrefix = databaseSchemaSettings$tablePrefix,
      tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
  }

  # add performance
  # =============
  if (!is.null(performanceEvaluation)) {
    addEvaluation(
      conn = conn,
      resultSchema = databaseSchemaSettings$resultSchema,
      targetDialect = databaseSchemaSettings$targetDialect,
      performanceId = performanceId,
      performanceEvaluation = performanceEvaluation,
      overWriteIfExists = TRUE,
      tablePrefix = databaseSchemaSettings$tablePrefix,
      tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
  }

  if (!is.null(covariateSummary)) {
    addCovariateSummary(
      conn = conn,
      resultSchema = databaseSchemaSettings$resultSchema,
      targetDialect = databaseSchemaSettings$targetDialect,
      performanceId = performanceId,
      covariateSummary = covariateSummary,
      restrictToIncluded = TRUE,
      overWriteIfExists = TRUE,
      tablePrefix = databaseSchemaSettings$tablePrefix,
      tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
    )
  }

  return(invisible(performanceId))
}


addPerformance <- function(
    conn,
    resultSchema,
    targetDialect,
    modelDesignId,
    developmentDatabaseId,
    validationDatabaseId,
    validationTargetId,
    validationOutcomeId,
    validationTarId,
    validationPlpDataId,
    validationPopulationId,
    modelDevelopment,
    executionDateTime,
    plpVersion,
    tablePrefix,
    tempEmulationSchema) {
  result <- checkTable(
    conn = conn,
    resultSchema = resultSchema,
    tablePrefix = tablePrefix,
    targetDialect = targetDialect,
    tableName = "performances",
    columnNames = c(
      "model_design_id",
      "development_database_id",
      "validation_database_id",
      "target_id",
      "outcome_id",
      "tar_id",
      "plp_data_setting_id",
      "population_setting_id",
      "model_development"
    ),
    values = c(
      modelDesignId,
      developmentDatabaseId,
      validationDatabaseId,
      validationTargetId,
      validationOutcomeId,
      validationTarId,
      validationPlpDataId,
      validationPopulationId,
      modelDevelopment
    ),
    tempEmulationSchema = tempEmulationSchema
  )

  if (nrow(result) == 0) {
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appendperformances (
    model_design_id,
    development_database_id,

    validation_database_id,
    target_id,
    outcome_id,
    tar_id,
    plp_data_setting_id,
    population_setting_id,

    model_development,

    execution_date_time,
    plp_version
  )
  VALUES (
  @model_design_id, @development_database_id,
  @validation_database_id, @validation_target_id, @validation_outcome_id, @validation_tar_id,
          @validation_plp_data_setting_id, @validation_population_setting_id,
          @model_development,
    '@execution_date_time', '@plp_version')"
    sql <- SqlRender::render(sql,
      my_schema = resultSchema,
      model_design_id = modelDesignId,
      development_database_id = developmentDatabaseId,
      validation_database_id = validationDatabaseId,
      validation_target_id = validationTargetId,
      validation_outcome_id = validationOutcomeId,
      validation_tar_id = validationTarId,
      validation_plp_data_setting_id = validationPlpDataId,
      validation_population_setting_id = validationPopulationId,
      model_development = modelDevelopment,
      execution_date_time = executionDateTime,
      plp_version = plpVersion,
      string_to_append = tablePrefix
    )
    sql <- SqlRender::translate(sql,
      targetDialect = targetDialect,
      tempEmulationSchema = tempEmulationSchema
    )
    DatabaseConnector::executeSql(conn, sql)

    # getId of new
    result <- checkTable(
      conn = conn,
      resultSchema = resultSchema,
      tablePrefix = tablePrefix,
      targetDialect = targetDialect,
      tableName = "performances",
      columnNames = c(
        "model_design_id",
        "development_database_id",
        "validation_database_id",
        "target_id",
        "outcome_id",
        "tar_id",
        "plp_data_setting_id",
        "population_setting_id",
        "model_development"
      ),
      values = c(
        modelDesignId,
        developmentDatabaseId,
        validationDatabaseId,
        validationTargetId,
        validationOutcomeId,
        validationTarId,
        validationPlpDataId,
        validationPopulationId,
        modelDevelopment
      ),
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(result$performanceId[1])
}

# attrition
addAttrition <- function(
    conn, resultSchema, targetDialect,
    tablePrefix = "",
    performanceId,
    attrition,
    overWriteIfExists = TRUE,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- attrition
  if (is.null(value)) {
    return(NULL)
  }

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)

  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "attrition"),
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "attrition"),
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        performance_id = performanceId,
        result_schema = resultSchema,
        table_name = paste0(tablePrefix, "attrition")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting attrition for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "attrition"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}


# evals
addEvaluation <- function(conn, resultSchema, targetDialect,
                          tablePrefix = "",
                          performanceId,
                          performanceEvaluation,
                          overWriteIfExists = TRUE,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  ParallelLogger::logInfo("Adding PredictionDistribution")
  tryCatch(
    {
      addPredictionDistribution(
        conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
        tablePrefix = tablePrefix,
        performanceId = performanceId,
        performanceEvaluation = performanceEvaluation,
        overWriteIfExists = overWriteIfExists,
        tempEmulationSchema = tempEmulationSchema
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding ThresholdSummary")
  tryCatch(
    {
      addThresholdSummary(
        conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
        tablePrefix = tablePrefix,
        performanceId = performanceId,
        performanceEvaluation = performanceEvaluation,
        overWriteIfExists = overWriteIfExists,
        tempEmulationSchema = tempEmulationSchema
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding EvaluationStatistics")
  tryCatch(
    {
      addEvaluationStatistics(
        conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
        tablePrefix = tablePrefix,
        performanceId = performanceId,
        performanceEvaluation = performanceEvaluation,
        overWriteIfExists = overWriteIfExists,
        tempEmulationSchema = tempEmulationSchema
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding CalibrationSummary")
  tryCatch(
    {
      addCalibrationSummary(
        conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
        tablePrefix = tablePrefix,
        performanceId = performanceId,
        performanceEvaluation = performanceEvaluation,
        overWriteIfExists = overWriteIfExists,
        tempEmulationSchema = tempEmulationSchema
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  ParallelLogger::logInfo("Adding DemographicSummary")
  tryCatch(
    {
      addDemographicSummary(
        conn = conn, resultSchema = resultSchema, targetDialect = targetDialect,
        tablePrefix = tablePrefix,
        performanceId = performanceId,
        performanceEvaluation = performanceEvaluation,
        overWriteIfExists = overWriteIfExists,
        tempEmulationSchema = tempEmulationSchema
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
    }
  )

  return(invisible(NULL))
}

addPredictionDistribution <- function(conn, resultSchema, targetDialect,
                                      tablePrefix = "",
                                      performanceId,
                                      performanceEvaluation,
                                      overWriteIfExists = TRUE,
                                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- performanceEvaluation$predictionDistribution
  if (is.null(value)) {
    return(NULL)
  }

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)
  if (sum(colnames(value) == "class") > 0) {
    colnames(value)[colnames(value) == "class"] <- "classLabel"
  }

  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "prediction_distribution"),
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "prediction_distribution"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        performance_id = performanceId,
        result_schema = resultSchema,
        table_name = paste0(tablePrefix, "prediction_distribution")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting predictionDistribution for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "prediction_distribution"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}

addThresholdSummary <- function(conn, resultSchema, targetDialect,
                                tablePrefix = "",
                                performanceId,
                                performanceEvaluation,
                                overWriteIfExists = TRUE,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- performanceEvaluation$thresholdSummary
  if (is.null(value)) {
    return(NULL)
  }

  #  check numerical columns:
  value <- cleanNum(value)

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)
  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tablePrefix = tablePrefix,
    tableName = "threshold_summary",
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "threshold_summary"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        result_schema = resultSchema,
        performance_id = performanceId,
        table_name = paste0(tablePrefix, "threshold_summary")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting thresholdSummary for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "threshold_summary"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}


addCalibrationSummary <- function(conn, resultSchema, targetDialect,
                                  tablePrefix = "",
                                  performanceId,
                                  performanceEvaluation,
                                  overWriteIfExists = TRUE,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- performanceEvaluation$calibrationSummary
  if (is.null(value)) {
    return(NULL)
  }

  #  check numerical columns:
  value <- cleanNum(value)

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)

  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tablePrefix = tablePrefix,
    tableName = "calibration_summary",
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "calibration_summary"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        result_schema = resultSchema,
        performance_id = performanceId,
        table_name = paste0(tablePrefix, "calibration_summary")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting calibrationSummary for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "calibration_summary"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}

addEvaluationStatistics <- function(conn, resultSchema, targetDialect,
                                    tablePrefix = "",
                                    performanceId,
                                    performanceEvaluation,
                                    overWriteIfExists = TRUE,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- data.frame(
    evaluation = unlist(performanceEvaluation$evaluationStatistics$evaluation),
    metric = unlist(performanceEvaluation$evaluationStatistics$metric),
    value = as.numeric(unlist(performanceEvaluation$evaluationStatistics$value))
  )

  if (is.null(value)) {
    return(NULL)
  }

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)
  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tablePrefix = tablePrefix,
    tableName = "evaluation_statistics",
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "evaluation_statistics"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        result_schema = resultSchema,
        performance_id = performanceId,
        table_name = paste0(tablePrefix, "evaluation_statistics")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting evaluationSummary for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "evaluation_statistics"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}

addDemographicSummary <- function(conn, resultSchema, targetDialect,
                                  tablePrefix = "",
                                  performanceId,
                                  performanceEvaluation,
                                  overWriteIfExists = TRUE,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- performanceEvaluation$demographicSummary
  if (is.null(value)) {
    return(NULL)
  }

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)

  value$performanceId <- performanceId

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tablePrefix = tablePrefix,
    tableName = "demographic_summary",
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "demographic_summary"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        result_schema = resultSchema,
        performance_id = performanceId,
        table_name = paste0(tablePrefix, "demographic_summary")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting demographicSummary for performance ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "demographic_summary"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}

addCovariateSummary <- function(conn, resultSchema, targetDialect,
                                tablePrefix = "",
                                performanceId,
                                covariateSummary,
                                restrictToIncluded = TRUE,
                                overWriteIfExists = TRUE,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  value <- covariateSummary
  if (is.null(value)) {
    return(NULL)
  }

  # edit names
  firstLower <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    return(x)
  }
  colnames(value) <- sapply(colnames(value), firstLower)
  value$performanceId <- performanceId
  # remove _ from names
  colnames(value) <- gsub("_", "", colnames(value))

  if (restrictToIncluded) {
    ParallelLogger::logInfo("Restricting to covariates included in model")
    value <- value[value$covariateValue != 0 & !is.na(value$covariateValue), ]
  }

  # get column names and check all present in object
  columnNames <- getColumnNames(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tablePrefix = tablePrefix,
    tableName = "covariate_summary",
    tempEmulationSchema = tempEmulationSchema
  )
  isValid <- sum(colnames(value) %in% columnNames) == length(columnNames)

  exists <- checkResultExists(
    conn = conn,
    resultSchema = resultSchema,
    targetDialect = targetDialect,
    tableName = paste0(tablePrefix, "covariate_summary"),
    resultIdName = "performance_id",
    resultId = performanceId,
    tempEmulationSchema = tempEmulationSchema
  )

  if (isValid && (!exists || overWriteIfExists)) {
    # REMOVE existing result
    if (exists) {
      ParallelLogger::logTrace("Removing existing covariateSummary")
      sql <- "delete from @result_schema.@table_name where performance_id = @performance_id;"
      sql <- SqlRender::render(sql,
        result_schema = resultSchema,
        performance_id = performanceId,
        table_name = paste0(tablePrefix, "covariate_summary")
      )
      sql <- SqlRender::translate(sql,
        targetDialect = targetDialect,
        tempEmulationSchema = tempEmulationSchema
      )
      DatabaseConnector::executeSql(conn, sql)
    }

    # add
    ParallelLogger::logInfo(paste0("Inserting covariateSummary for result ", performanceId))
    DatabaseConnector::insertTable(
      connection = conn,
      databaseSchema = resultSchema,
      tableName = paste0(tablePrefix, "covariate_summary"),
      data = value[, columnNames],
      dropTableIfExists = FALSE, createTable = FALSE, tempTable = FALSE,
      bulkLoad = FALSE, camelCaseToSnakeCase = TRUE, progressBar = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  return(invisible(NULL))
}



# ====================
# Helpers
# ====================

# gets the column names in camelCase of a table
getColumnNames <- function(conn, resultSchema, targetDialect, tableName, tablePrefix = "",
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  sql <- "select top 1 * from @my_schema.@string_to_append@table;"
  sql <- SqlRender::render(sql,
    my_schema = resultSchema,
    table = tableName,
    string_to_append = tablePrefix
  )
  sql <- SqlRender::translate(sql,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema
  )
  result <- DatabaseConnector::querySql(connection = conn, sql = sql, snakeCaseToCamelCase = TRUE)

  return(colnames(result))
}

# True/False check whether results exist in table
checkResultExists <- function(conn, resultSchema, targetDialect,
                              snakeCaseToCamelCase,
                              tableName,
                              resultIdName = "performance_id",
                              resultId,
                              tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  sql <- "select * from @my_schema.@table where @result_id_name = @result_id;"
  sql <- SqlRender::render(sql,
    my_schema = resultSchema,
    table = tableName,
    result_id_name = resultIdName,
    result_id = resultId
  )
  sql <- SqlRender::translate(sql,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema
  )
  result <- DatabaseConnector::querySql(connection = conn, sql = sql, snakeCaseToCamelCase = TRUE)
  return(nrow(result) > 0)
}
