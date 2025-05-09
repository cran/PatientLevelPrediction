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

test_that("getDemographicSummary", {
  prediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(100, 100, replace = TRUE),
    gender = sample(c(8507, "female"), 100, replace = TRUE),
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100)
  )

  demoSum <- getDemographicSummary(
    prediction = prediction,
    predictionType = "binary",
    typeColumn = "evaluation"
  )

  expect_equal(ncol(demoSum), 12)
  expect_true("evaluation" %in% colnames(demoSum))

  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSum$genGroup)))


  demoSumBin <- getDemographicSummary_binary(
    prediction = prediction,
    evalColumn = "evaluation"
  )
  expect_equal(demoSum, demoSumBin)
})


test_that("getDemographicSummary", {
  prediction <- data.frame(
    rowId = 1:100,
    ageYear = sample(100, 100, replace = TRUE),
    gender = sample(c(8507, "female"), 100, replace = TRUE),
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100),
    survivalTime = 50 + sample(730, 100, replace = TRUE)
  )

  demoSumSurv <- getDemographicSummary_survival(
    prediction = prediction,
    evalColumn = "evaluation",
    timepoint = 365
  )

  expect_s3_class(demoSumSurv, "data.frame")
  expect_equal(ncol(demoSumSurv), 8)
  expect_true("evaluation" %in% colnames(demoSumSurv))

  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSumSurv$genGroup)))
})
