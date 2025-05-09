# @file SklearnClassifierSettings.R
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

#' Create setting for AdaBoost with python DecisionTreeClassifier base estimator
#' @param nEstimators    (list) The maximum number of estimators at which boosting is terminated. In case of perfect fit, the learning procedure is stopped early.
#' @param learningRate   (list) Weight applied to each classifier at each boosting iteration. A higher learning rate increases the contribution of each classifier. There is a trade-off between the learningRate and nEstimators parameters
#'                       There is a trade-off between learningRate and nEstimators.
#' @param algorithm      Only ‘SAMME’ can be provided. The 'algorithm' argument will be deprecated in scikit-learn 1.8.
#' @param seed           A seed for the model
#' @return a modelSettings object
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
#' \dontrun{ 
#' model <- setAdaBoost(nEstimators = list(10),
#'                      learningRate = list(0.1),
#'                      seed = 42)
#' }
#' @export
setAdaBoost <- function(nEstimators = list(10, 50, 200),
                        learningRate = list(1, 0.5, 0.1),
                        algorithm = list("SAMME"),
                        seed = sample(1000000, 1)) {
  checkSklearn()
  checkIsClass(seed[[1]], c("numeric", "integer"))
  checkIsClass(nEstimators, "list")
  checkIsClass(learningRate, "list")
  checkIsClass(algorithm, "list")

  lapply(1:length(nEstimators), function(i) {
    checkIsClass(nEstimators[[i]], c("integer", "numeric"))
  })
  lapply(1:length(nEstimators), function(i) {
    checkHigher(nEstimators[[i]], 0)
  })

  for (i in 1:length(nEstimators)) {
    if (inherits(x = nEstimators[[i]], what = c("numeric", "integer"))) {
      nEstimators[[i]] <- as.integer(nEstimators[[i]])
    }
  }

  lapply(1:length(learningRate), function(i) {
    checkIsClass(learningRate[[i]], c("numeric"))
  })
  lapply(1:length(learningRate), function(i) {
    checkHigher(learningRate[[i]], 0)
  })

  lapply(1:length(algorithm), function(i) {
    checkIsClass(algorithm[[i]], c("character"))
    checkIsEqual(algorithm[[i]], "SAMME")
  })

  paramGrid <- list(
    nEstimators = nEstimators,
    learningRate = learningRate,
    algorithm = algorithm,
    seed = list(as.integer(seed[[1]]))
  )

  param <- listCartesian(paramGrid)

  attr(param, "settings") <- list(
    modelType = "adaBoost",
    seed = seed[[1]],
    paramNames = names(paramGrid),
    # use this for logging params
    requiresDenseMatrix = FALSE,
    name = "AdaBoost",
    pythonModule = "sklearn.ensemble",
    pythonClass = "AdaBoostClassifier"
  )

  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}


AdaBoostClassifierInputs <- function(classifier, param) {
  model <- classifier(
    n_estimators = param[[which.max(names(param) == "nEstimators")]],
    learning_rate = param[[which.max(names(param) == "learningRate")]],
    algorithm = param[[which.max(names(param) == "algorithm")]],
    random_state = param[[which.max(names(param) == "seed")]]
  )

  return(model)
}

#' Create setting for the scikit-learn DecisionTree with python
#' @param criterion The function to measure the quality of a split. Supported criteria are “gini” for the Gini impurity and “entropy” for the information gain.
#' @param splitter The strategy used to choose the split at each node. Supported strategies are “best” to choose the best split and “random” to choose the best random split.
#' @param maxDepth    (list) The maximum depth of the tree. If NULL, then nodes are expanded until all leaves are pure or until all leaves contain less than min_samples_split samples.
#' @param minSamplesSplit    The minimum number of samples required to split an internal node
#' @param minSamplesLeaf     The minimum number of samples required to be at a leaf node. A split point at any depth will only be considered if it leaves at least minSamplesLeaf training samples in each of the left and right branches. This may have the effect of smoothing the model, especially in regression.
#' @param minWeightFractionLeaf    The minimum weighted fraction of the sum total of weights (of all the input samples) required to be at a leaf node. Samples have equal weight when sampleWeight is not provided.
#' @param maxFeatures   (list) The number of features to consider when looking for the best split (int/'sqrt'/NULL)
#' @param maxLeafNodes (list) Grow a tree with max_leaf_nodes in best-first fashion. Best nodes are defined as relative reduction in impurity. If None then unlimited number of leaf nodes. (int/NULL)
#' @param minImpurityDecrease  Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf.
#' @param classWeight         (list) Weights associated with classes 'balance' or NULL
#' @param seed                The random state seed
#' @return a modelSettings object
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
#' \dontrun{ 
#' model <- setDecisionTree(criterion = list("gini"),
#'                          maxDepth = list(4),
#'                          minSamplesSplit = list(2),
#'                          minSamplesLeaf = list(10),
#'                          seed = 42)
#' }
#' @export
setDecisionTree <- function(criterion = list("gini"),
                            splitter = list("best"),
                            maxDepth = list(as.integer(4), as.integer(10), NULL),
                            minSamplesSplit = list(2, 10),
                            minSamplesLeaf = list(10, 50),
                            minWeightFractionLeaf = list(0),
                            maxFeatures = list(100, "sqrt", NULL),
                            maxLeafNodes = list(NULL),
                            minImpurityDecrease = list(10^-7),
                            classWeight = list(NULL),
                            seed = sample(1000000, 1)) {
  checkSklearn()
  checkIsClass(criterion, "list")
  checkIsClass(splitter, "list")
  checkIsClass(maxDepth, "list")
  checkIsClass(minSamplesSplit, "list")
  checkIsClass(minSamplesLeaf, "list")
  checkIsClass(minWeightFractionLeaf, "list")
  checkIsClass(maxFeatures, "list")
  checkIsClass(maxLeafNodes, "list")
  checkIsClass(minImpurityDecrease, "list")
  checkIsClass(classWeight, "list")

  lapply(1:length(criterion), function(i) {
    checkIsClass(criterion[[i]], "character")
  })
  lapply(1:length(splitter), function(i) {
    checkIsClass(splitter[[i]], "character")
  })


  lapply(
    1:length(criterion),
    function(i) {
      if (!criterion[[i]] %in% c("gini", "entropy")) {
        stop("Incorrect criterion")
      }
    }
  )


  lapply(1:length(maxDepth), function(i) {
    checkIsClass(maxDepth[[i]], c("numeric", "integer", "NULL"))
  })
  lapply(1:length(maxDepth), function(i) {
    checkHigher(ifelse(is.null(maxDepth[[i]]), 1, maxDepth[[i]]), 0)
  })
  for (i in 1:length(maxDepth)) {
    if (inherits(x = maxDepth[[i]], what = c("numeric", "integer"))) {
      maxDepth[[i]] <- as.integer(maxDepth[[i]])
    }
  }

  lapply(
    1:length(minSamplesSplit),
    function(i) {
      checkIsClass(minSamplesSplit[[i]], c("numeric", "integer", "NULL"))
    }
  )
  lapply(
    1:length(minSamplesSplit),
    function(i) {
      checkHigher(ifelse(
        is.null(minSamplesSplit[[i]]), 1, minSamplesSplit[[i]]
      ), 0)
    }
  )

  # convert to integer if >= 1
  for (i in 1:length(minSamplesSplit)) {
    if (minSamplesSplit[[i]] >= 1) {
      minSamplesSplit[[i]] <- as.integer(minSamplesSplit[[i]])
    }
  }


  lapply(
    1:length(minSamplesLeaf),
    function(i) {
      checkIsClass(minSamplesLeaf[[i]], c("numeric", "integer"))
    }
  )
  lapply(
    1:length(minSamplesLeaf),
    function(i) {
      checkHigher(minSamplesLeaf[[i]], 0)
    }
  )

  # convert to integer if >= 1
  for (i in 1:length(minSamplesLeaf)) {
    if (minSamplesLeaf[[i]] >= 1) {
      minSamplesLeaf[[i]] <- as.integer(minSamplesLeaf[[i]])
    }
  }

  lapply(
    1:length(minWeightFractionLeaf),
    function(i) {
      checkIsClass(minWeightFractionLeaf[[i]], c("numeric"))
    }
  )
  lapply(
    1:length(minWeightFractionLeaf),
    function(i) {
      checkHigherEqual(minWeightFractionLeaf[[i]], 0)
    }
  )

  lapply(
    1:length(maxFeatures),
    function(i) {
      checkIsClass(maxFeatures[[i]], c(
        "numeric", "integer", "character", "NULL"
      ))
    }
  )

  for (i in 1:length(maxFeatures)) {
    if (inherits(x = maxFeatures[[i]], what = c("numeric", "integer"))) {
      maxFeatures[[i]] <- as.integer(maxFeatures[[i]])
    }
  }

  lapply(
    1:length(maxLeafNodes),
    function(i) {
      checkIsClass(maxLeafNodes[[i]], c("integer", "NULL"))
    }
  )
  lapply(
    1:length(maxLeafNodes),
    function(i) {
      checkHigher(ifelse(
        is.null(maxLeafNodes[[i]]), 1, maxLeafNodes[[i]]
      ), 0)
    }
  )

  for (i in 1:length(maxLeafNodes)) {
    if (inherits(x = maxLeafNodes[[i]], what = c("numeric", "integer"))) {
      maxLeafNodes[[i]] <- as.integer(maxLeafNodes[[i]])
    }
  }

  lapply(
    1:length(minImpurityDecrease),
    function(i) {
      checkIsClass(minImpurityDecrease[[i]], c("numeric"))
    }
  )
  lapply(
    1:length(minImpurityDecrease),
    function(i) {
      checkHigherEqual(minImpurityDecrease[[i]], 0)
    }
  )

  lapply(1:length(classWeight), function(i) {
    checkIsClass(classWeight[[i]], c("character", "NULL"))
  })

  paramGrid <- list(
    criterion = criterion,
    splitter = splitter,
    maxDepth = maxDepth,
    minSamplesSplit = minSamplesSplit,
    minSamplesLeaf = minSamplesLeaf,
    minWeightFractionLeaf = minWeightFractionLeaf,
    maxFeatures = maxFeatures,
    seed = list(seed[[1]]),
    maxLeafNodes = maxLeafNodes,
    minImpurityDecrease = minImpurityDecrease,
    classWeight = classWeight
  )
  param <- listCartesian(paramGrid)

  attr(param, "settings") <- list(
    modelType = "decisionTree",
    seed = seed[[1]],
    paramNames = names(paramGrid),
    requiresDenseMatrix = FALSE,
    name = "Decision Tree",
    pythonModule = "sklearn.tree",
    pythonClass = "DecisionTreeClassifier"
  )

  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}


DecisionTreeClassifierInputs <- function(classifier, param) {
  model <- classifier(
    criterion = param[[which.max(names(param) == "criterion")]],
    splitter = param[[which.max(names(param) == "splitter")]],
    max_depth = param[[which.max(names(param) == "maxDepth")]],
    min_samples_split = param[[which.max(names(param) == "minSamplesSplit")]],
    min_samples_leaf = param[[which.max(names(param) == "minSamplesLeaf")]],
    min_weight_fraction_leaf = param[[which.max(names(param) == "minWeightFractionLeaf")]],
    max_features = param[[which.max(names(param) == "maxFeatures")]],
    random_state = param[[which.max(names(param) == "seed")]],
    max_leaf_nodes = param[[which.max(names(param) == "maxLeafNodes")]],
    min_impurity_decrease = param[[which.max(names(param) == "minImpurityDecrease")]],
    class_weight = param[[which.max(names(param) == "classWeight")]]
  )

  return(model)
}


#' Create setting for neural network model with python's scikit-learn. For 
#' bigger models, consider using `DeepPatientLevelPrediction` package.
#'
#' @param hiddenLayerSizes      (list of vectors) The ith element represents the number of neurons in the ith hidden layer.
#' @param activation (list) Activation function for the hidden layer.
#' \itemize{
#'    \item "identity": no-op activation, useful to implement linear bottleneck, returns f(x) = x
#'    \item "logistic": the logistic sigmoid function, returns f(x) = 1 / (1 + exp(-x)).
#'    \item "tanh": the hyperbolic tan function, returns f(x) = tanh(x).
#'    \item "relu": the rectified linear unit function, returns f(x) = max(0, x)
#'  }
#' @param solver     (list) The solver for weight optimization. (‘lbfgs’, ‘sgd’, ‘adam’)
#' @param alpha      (list) L2 penalty (regularization term) parameter.
#' @param batchSize  (list) Size of minibatches for stochastic optimizers. If the solver is ‘lbfgs’, the classifier will not use minibatch. When set to “auto”, batchSize=min(200, n_samples).
#' @param learningRate (list) Only used when solver='sgd' Learning rate schedule for weight updates. ‘constant’, ‘invscaling’, ‘adaptive’, default=’constant’
#' @param learningRateInit  (list) Only used when solver=’sgd’ or ‘adam’. The initial learning rate used. It controls the step-size in updating the weights.
#' @param powerT     (list) Only used when solver=’sgd’.  The exponent for inverse scaling learning rate. It is used in updating effective learning rate when the learning_rate is set to ‘invscaling’.
#' @param maxIter  (list)  Maximum number of iterations. The solver iterates until convergence (determined by ‘tol’) or this number of iterations. For stochastic solvers (‘sgd’, ‘adam’), note that this determines the number of epochs (how many times each data point will be used), not the number of gradient steps.
#' @param shuffle (list) boolean: Whether to shuffle samples in each iteration. Only used when solver=’sgd’ or ‘adam’.
#' @param tol        (list) Tolerance for the optimization. When the loss or score is not improving by at least tol for nIterNoChange consecutive iterations, unless learning_rate is set to ‘adaptive’, convergence is considered to be reached and training stops.
#' @param warmStart (list) When set to True, reuse the solution of the previous call to fit as initialization, otherwise, just erase the previous solution.
#' @param momentum (list) Momentum for gradient descent update. Should be between 0 and 1. Only used when solver=’sgd’.
#' @param nesterovsMomentum (list) Whether to use Nesterov’s momentum. Only used when solver=’sgd’ and momentum > 0.
#' @param earlyStopping (list) boolean Whether to use early stopping to terminate training when validation score is not improving. If set to true, it will automatically set aside 10 percent of training data as validation and terminate training when validation score is not improving by at least tol for n_iter_no_change consecutive epochs.
#' @param validationFraction (list) The proportion of training data to set aside as validation set for early stopping. Must be between 0 and 1. Only used if earlyStopping is True.
#' @param beta1    (list) Exponential decay rate for estimates of first moment vector in adam, should be in 0 to 1.
#' @param beta2    (list) Exponential decay rate for estimates of second moment vector in adam, should be in 0 to 1.
#' @param epsilon  (list) Value for numerical stability in adam.
#' @param nIterNoChange     (list) Maximum number of epochs to not meet tol improvement. Only effective when solver=’sgd’ or ‘adam’.
#' @param seed       A seed for the model
#' @return a modelSettings object
#'
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it } 
#' \dontrun{ 
#' model <- setMLP(hiddenLayerSizes = list(c(20)), alpha=list(3e-4), seed = 42)
#' }
#' @export
setMLP <- function(hiddenLayerSizes = list(c(100), c(20)),
                   # must be integers
                   activation = list("relu"),
                   solver = list("adam"),
                   alpha = list(0.3, 0.01, 0.0001, 0.000001),
                   batchSize = list("auto"),
                   learningRate = list("constant"),
                   learningRateInit = list(0.001),
                   powerT = list(0.5),
                   maxIter = list(200, 100),
                   shuffle = list(TRUE),
                   tol = list(0.0001),
                   warmStart = list(TRUE),
                   momentum = list(0.9),
                   nesterovsMomentum = list(TRUE),
                   earlyStopping = list(FALSE),
                   validationFraction = list(0.1),
                   beta1 = list(0.9),
                   beta2 = list(0.999),
                   epsilon = list(0.00000001),
                   nIterNoChange = list(10),
                   seed = sample(100000, 1)) {
  checkSklearn()
  checkIsClass(seed, c("numeric", "integer"))
  checkIsClass(hiddenLayerSizes, c("list"))
  checkIsClass(activation, c("list"))
  checkIsClass(solver, c("list"))
  checkIsClass(alpha, c("list"))
  checkIsClass(batchSize, c("list"))
  checkIsClass(learningRate, c("list"))
  checkIsClass(learningRateInit, c("list"))
  checkIsClass(powerT, c("list"))
  checkIsClass(maxIter, c("list"))
  checkIsClass(shuffle, c("list"))
  checkIsClass(tol, c("list"))
  checkIsClass(warmStart, c("list"))
  checkIsClass(momentum, c("list"))
  checkIsClass(nesterovsMomentum, c("list"))
  checkIsClass(earlyStopping, c("list"))
  checkIsClass(validationFraction, c("list"))
  checkIsClass(beta1, c("list"))
  checkIsClass(beta2, c("list"))
  checkIsClass(epsilon, c("list"))
  checkIsClass(nIterNoChange, c("list"))


  for (i in 1:length(hiddenLayerSizes)) {
    hiddenLayerSizes[[i]] <- as.integer(hiddenLayerSizes[[i]])
  }


  for (i in 1:length(batchSize)) {
    if (inherits(x = batchSize[[i]], what = c("numeric", "integer"))) {
      batchSize[[i]] <- as.integer(batchSize[[i]])
    }
  }

  for (i in 1:length(maxIter)) {
    if (inherits(x = maxIter[[i]], what = c("numeric", "integer"))) {
      maxIter[[i]] <- as.integer(maxIter[[i]])
    }
  }

  for (i in 1:length(nIterNoChange)) {
    if (inherits(x = nIterNoChange[[i]], what = c("numeric", "integer"))) {
      nIterNoChange[[i]] <- as.integer(nIterNoChange[[i]])
    }
  }

  # add lapply for values...
  paramGrid <- list(
    hiddenLayerSizes = hiddenLayerSizes,
    activation = activation,
    solver = solver,
    alpha = alpha,
    batchSize = batchSize,
    learningRate = learningRate,
    learningRateInit = learningRateInit,
    powerT = powerT,
    maxIter = maxIter,
    shuffle = shuffle,
    seed = list(as.integer(seed[[1]])),
    tol = tol,
    verbosebool = list(FALSE),
    warmStart = warmStart,
    momentum = momentum,
    nesterovsMomentum = nesterovsMomentum,
    earlyStopping = earlyStopping,
    validationFraction = validationFraction,
    beta1 = beta1,
    beta2 = beta2,
    epsilon = epsilon,
    nIterNoChange = nIterNoChange
  )

  param <- listCartesian(paramGrid)

  attr(param, "settings") <- list(
    modelType = "mlp",
    seed = seed[[1]],
    paramNames = names(paramGrid),
    # use this for logging params
    requiresDenseMatrix = FALSE,
    name = "Neural Network",
    pythonModule = "sklearn.neural_network",
    pythonClass = "MLPClassifier"
  )

  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}

MLPClassifierInputs <- function(classifier, param) {
  model <- classifier(
    hidden_layer_sizes = param[[which.max(names(param) == "hiddenLayerSizes")]],
    activation = param[[which.max(names(param) == "activation")]],
    solver = param[[which.max(names(param) == "solver")]],
    alpha = param[[which.max(names(param) == "alpha")]],
    batch_size = param[[which.max(names(param) == "batchSize")]],
    learning_rate = param[[which.max(names(param) == "learningRate")]],
    learning_rate_init = param[[which.max(names(param) == "learningRateInit")]],
    power_t = param[[which.max(names(param) == "powerT")]],
    max_iter = param[[which.max(names(param) == "maxIter")]],
    shuffle = param[[which.max(names(param) == "shuffle")]],
    random_state = param[[which.max(names(param) == "seed")]],
    tol = param[[which.max(names(param) == "tol")]],
    verbose = FALSE,
    warm_start = param[[which.max(names(param) == "warmStart")]],
    momentum = param[[which.max(names(param) == "momentum")]],
    nesterovs_momentum = param[[which.max(names(param) == "nesterovsMomentum")]],
    early_stopping = param[[which.max(names(param) == "earlyStopping")]],
    validation_fraction = param[[which.max(names(param) == "validationFraction")]],
    beta_1 = param[[which.max(names(param) == "beta1")]],
    beta_2 = param[[which.max(names(param) == "beta2")]],
    epsilon = param[[which.max(names(param) == "epsilon")]],
    n_iter_no_change = param[[which.max(names(param) == "nIterNoChange")]]
  )

  return(model)
}



#' Create setting for naive bayes model with python
#'
#' @return a modelSettings object
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
#' \dontrun{ 
#' plpData <- getEunomiaPlpData()
#' model <- setNaiveBayes()
#' analysisId <- "naiveBayes"
#' saveLocation <- file.path(tempdir(), analysisId)
#' results <- runPlp(plpData, modelSettings = model,
#'                   saveDirectory = saveLocation,
#'                   analysisId = analysisId)
#' # clean up
#' unlink(saveLocation, recursive = TRUE)
#' }
#' @export
setNaiveBayes <- function() {
  param <- list(none = "true")

  attr(param, "settings") <- list(
    modelType = "naiveBayes",
    seed = as.integer(0),
    paramNames = c(),
    # use this for logging params
    requiresDenseMatrix = TRUE,
    name = "Naive Bayes",
    pythonModule = "sklearn.naive_bayes",
    pythonClass = "GaussianNB"
  )
  checkSklearn()
  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}

GaussianNBInputs <- function(classifier, param) {
  model <- classifier()
  return(model)
}


#' Create setting for random forest model using sklearn
#'
#' @param ntrees    (list) The number of trees to build
#' @param criterion (list) The function to measure the quality of a split. Supported criteria are “gini” for the Gini impurity and “entropy” for the information gain. Note: this parameter is tree-specific.
#' @param maxDepth   (list) The maximum depth of the tree. If NULL, then nodes are expanded until all leaves are pure or until all leaves contain less than minSamplesSplit samples.
#' @param minSamplesSplit (list) The minimum number of samples required to split an internal node
#' @param minSamplesLeaf (list) The minimum number of samples required to be at a leaf node. A split point at any depth will only be considered if it leaves at least minSamplesLeaf training samples in each of the left and right branches. This may have the effect of smoothing the model, especially in regression.
#' @param minWeightFractionLeaf (list) The minimum weighted fraction of the sum total of weights (of all the input samples) required to be at a leaf node. Samples have equal weight when sampleWeight is not provided.
#' @param mtries (list) The number of features to consider when looking for the best split:
#' \itemize{
#' \item int then consider max_features features at each split.
#' \item float then max_features is a fraction and round(max_features * n_features) features are considered at each split
#' \item 'sqrt' then max_features=sqrt(n_features)
#' \item 'log2' then max_features=log2(n_features)
#' \item NULL then max_features=n_features
#' }
#' @param maxLeafNodes    (list) Grow trees with max_leaf_nodes in best-first fashion. Best nodes are defined as relative reduction in impurity. If None then unlimited number of leaf nodes.
#' @param minImpurityDecrease (list) A node will be split if this split induces a decrease of the impurity greater than or equal to this value.
#' @param bootstrap (list) Whether bootstrap samples are used when building trees. If False, the whole dataset is used to build each tree.
#' @param maxSamples (list) If bootstrap is True, the number of samples to draw from X to train each base estimator.
#' @param oobScore (list) Whether to use out-of-bag samples to estimate the generalization score. Only available if bootstrap=True.
#' @param classWeight (list) Weights associated with classes. If not given, all classes are supposed to have weight one. NULL, “balanced”, “balanced_subsample”
#' @param nJobs The number of jobs to run in parallel.
#' @param seed  A seed when training the final model
#' @return a modelSettings object
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
#' \dontrun{ 
#' plpData <- getEunomiaPlpData()
#' model <- setRandomForest(ntrees = list(100),
#'                           maxDepth = list(4),
#'                           minSamplesSplit = list(2),
#'                           minSamplesLeaf = list(10),
#'                           maxSamples = list(0.9),
#'                           seed = 42)
#' saveLoc <- file.path(tempdir(), "randomForest")
#' results <- runPlp(plpData, modelSettings = model, saveDirectory = saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
setRandomForest <- function(ntrees = list(100, 500),
                            criterion = list("gini"),
                            maxDepth = list(4, 10, 17),
                            minSamplesSplit = list(2, 5),
                            minSamplesLeaf = list(1, 10),
                            minWeightFractionLeaf = list(0),
                            mtries = list("sqrt", "log2"),
                            maxLeafNodes = list(NULL),
                            minImpurityDecrease = list(0),
                            bootstrap = list(TRUE),
                            maxSamples = list(NULL, 0.9),
                            oobScore = list(FALSE),
                            nJobs = list(NULL),
                            classWeight = list(NULL),
                            seed = sample(100000, 1)) {
  checkSklearn()
  checkIsClass(seed, c("numeric", "integer"))
  checkIsClass(ntrees, c("list"))
  checkIsClass(criterion, c("list"))
  checkIsClass(maxDepth, c("list"))
  checkIsClass(minSamplesSplit, c("list"))
  checkIsClass(minSamplesLeaf, c("list"))
  checkIsClass(minWeightFractionLeaf, c("list"))
  checkIsClass(mtries, c("list"))
  checkIsClass(maxLeafNodes, c("list"))
  checkIsClass(minImpurityDecrease, c("list"))
  checkIsClass(bootstrap, c("list"))
  checkIsClass(maxSamples, c("list"))
  checkIsClass(oobScore, c("list"))
  checkIsClass(nJobs, c("list"))
  checkIsClass(classWeight, c("list"))

  # convert to integer when needed
  for (i in 1:length(ntrees)) {
    if (inherits(x = ntrees[[i]], what = c("numeric", "integer"))) {
      ntrees[[i]] <- as.integer(ntrees[[i]])
    }
  }
  for (i in 1:length(maxDepth)) {
    if (inherits(x = maxDepth[[i]], what = c("numeric", "integer"))) {
      maxDepth[[i]] <- as.integer(maxDepth[[i]])
    }
  }

  for (i in 1:length(minSamplesSplit)) {
    if (minSamplesSplit[[i]] >= 1) {
      minSamplesSplit[[i]] <- as.integer(minSamplesSplit[[i]])
    }
  }

  for (i in 1:length(minSamplesLeaf)) {
    if (minSamplesLeaf[[i]] >= 1) {
      minSamplesLeaf[[i]] <- as.integer(minSamplesLeaf[[i]])
    }
  }

  for (i in 1:length(maxLeafNodes)) {
    if (inherits(x = maxLeafNodes[[i]], what = c("numeric", "integer"))) {
      maxLeafNodes[[i]] <- as.integer(maxLeafNodes[[i]])
    }
  }

  for (i in 1:length(nJobs)) {
    if (inherits(x = nJobs[[i]], what = c("numeric", "integer"))) {
      nJobs[[i]] <- as.integer(nJobs[[i]])
    }
  }

  for (i in 1:length(maxSamples)) {
    if (inherits(x = maxSamples[[i]], what = c("numeric", "integer"))) {
      if (maxSamples[[i]] >= 1) {
        maxSamples[[i]] <- as.integer(maxSamples[[i]])
      }
    }
  }

  # add value checks
  paramGrid <- list(
    ntrees = ntrees,
    criterion = criterion,
    maxDepth = maxDepth,
    minSamplesSplit = minSamplesSplit,
    minSamplesLeaf = minSamplesLeaf,
    minWeightFractionLeaf = minWeightFractionLeaf,
    mtries = mtries,
    maxLeafNodes = maxLeafNodes,
    minImpurityDecrease = minImpurityDecrease,
    bootstrap = bootstrap,
    oobScore = oobScore,
    nJobs = nJobs,
    seed = list(as.integer(seed[[1]])),
    classWeight = classWeight,
    maxSamples = maxSamples
  )
  param <- listCartesian(paramGrid)

  attr(param, "settings") <- list(
    modelType = "randomForest",
    seed = seed[[1]],
    paramNames = names(paramGrid),
    # use this for logging params
    requiresDenseMatrix = FALSE,
    name = "Random forest",
    pythonModule = "sklearn.ensemble",
    pythonClass = "RandomForestClassifier"
  )

  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}


RandomForestClassifierInputs <- function(classifier, param) {
  model <- classifier(
    n_estimators = param[[which.max(names(param) == "ntrees")]],
    criterion = param[[which.max(names(param) == "criterion")]],
    max_depth = param[[which.max(names(param) == "maxDepth")]],
    min_samples_split = param[[which.max(names(param) == "minSamplesSplit")]],
    min_samples_leaf = param[[which.max(names(param) == "minSamplesLeaf")]],
    min_weight_fraction_leaf = param[[which.max(names(param) == "minWeightFractionLeaf")]],
    max_features = param[[which.max(names(param) == "mtries")]],
    max_leaf_nodes = param[[which.max(names(param) == "maxLeafNodes")]],
    min_impurity_decrease = param[[which.max(names(param) == "minImpurityDecrease")]],
    bootstrap = param[[which.max(names(param) == "bootstrap")]],
    max_samples = param[[which.max(names(param) == "maxSamples")]],
    oob_score = param[[which.max(names(param) == "oobScore")]],
    n_jobs = param[[which.max(names(param) == "nJobs")]],
    random_state = param[[which.max(names(param) == "seed")]],
    verbose = 0L,
    warm_start = FALSE,
    class_weight = param[[which.max(names(param) == "classWeight")]]
  )

  return(model)
}



#' Create setting for the python sklearn SVM (SVC function)
#' @param C             (list) Regularization parameter. The strength of the regularization is inversely proportional to C. Must be strictly positive. The penalty is a squared l2 penalty.
#' @param kernel        (list) Specifies the kernel type to be used in the algorithm. one of ‘linear’, ‘poly’, ‘rbf’, ‘sigmoid’, ‘precomputed’. If none is given ‘rbf’ will be used.
#' @param degree        (list) degree of kernel function is significant only in poly, rbf, sigmoid
#' @param gamma         (list) kernel coefficient for rbf and poly, by default 1/n_features will be taken. ‘scale’, ‘auto’ or float, default=’scale’
#' @param coef0         (list) independent term in kernel function. It is only significant in poly/sigmoid.
#' @param shrinking     (list) whether to use the shrinking heuristic.
#' @param tol           (list) Tolerance for stopping criterion.
#' @param classWeight   (list) Class weight based on imbalance either 'balanced' or NULL
#' @param cacheSize     Specify the size of the kernel cache (in MB).
#' @param seed           A seed for the model
#' @return a modelSettings object
#' @examples
#' \dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
#' \dontrun{ 
#' plpData <- getEunomiaPlpData()
#' model <- setSVM(C = list(1), gamma = list("scale"), seed = 42)
#' saveLoc <- file.path(tempdir(), "svm")
#' results <- runPlp(plpData, modelSettings = model, saveDirectory = saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
setSVM <- function(C = list(1, 0.9, 2, 0.1),
                   kernel = list("rbf"),
                   degree = list(1, 3, 5),
                   gamma = list("scale", 1e-04, 3e-05, 0.001, 0.01, 0.25),
                   coef0 = list(0.0),
                   shrinking = list(TRUE),
                   tol = list(0.001),
                   classWeight = list(NULL),
                   cacheSize = 500,
                   seed = sample(100000, 1)) {
  checkSklearn()
  checkIsClass(seed, c("numeric", "integer"))
  checkIsClass(cacheSize, c("numeric", "integer"))
  checkIsClass(C, c("list"))
  checkIsClass(kernel, c("list"))
  checkIsClass(degree, c("list"))
  checkIsClass(gamma, c("list"))
  checkIsClass(coef0, c("list"))
  checkIsClass(shrinking, c("list"))
  checkIsClass(tol, c("list"))
  checkIsClass(classWeight, c("list"))

  for (i in 1:length(degree)) {
    if (inherits(x = degree[[i]], what = c("numeric", "integer"))) {
      degree[[i]] <- as.integer(degree[[i]])
    }
  }

  paramGrid <- list(
    C = C,
    kernel = kernel,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    shrinking = shrinking,
    tol = tol,
    cacheSize = list(cacheSize),
    classWeight = classWeight,
    seed = list(as.integer(seed[[1]]))
  )

  param <- listCartesian(paramGrid)


  attr(param, "settings") <- list(
    modelType = "svm",
    seed = seed[[1]],
    paramNames = names(paramGrid),
    # use this for logging params
    requiresDenseMatrix = FALSE,
    name = "Support Vector Machine",
    pythonModule = "sklearn.svm",
    pythonClass = "SVC"
  )

  attr(param, "saveToJson") <- TRUE
  attr(param, "saveType") <- "file"

  result <- list(
    fitFunction = "fitSklearn",
    param = param
  )
  class(result) <- "modelSettings"

  return(result)
}

SVCInputs <- function(classifier, param) {
  model <- classifier(
    C = param[[which.max(names(param) == "C")]],
    kernel = param[[which.max(names(param) == "kernel")]],
    degree = param[[which.max(names(param) == "degree")]],
    gamma = param[[which.max(names(param) == "gamma")]],
    coef0 = param[[which.max(names(param) == "coef0")]],
    shrinking = param[[which.max(names(param) == "shrinking")]],
    probability = TRUE,
    tol = param[[which.max(names(param) == "tol")]],
    cache_size = param[[which.max(names(param) == "cacheSize")]],
    class_weight = param[[which.max(names(param) == "classWeight")]],
    verbose = FALSE,
    max_iter = as.integer(-1),
    decision_function_shape = "ovr",
    break_ties = FALSE,
    random_state = param[[which.max(names(param) == "seed")]]
  )

  return(model)
}
