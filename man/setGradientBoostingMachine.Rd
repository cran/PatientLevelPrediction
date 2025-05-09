% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GradientBoostingMachine.R
\name{setGradientBoostingMachine}
\alias{setGradientBoostingMachine}
\title{Create setting for gradient boosting machine model using gbm_xgboost implementation}
\usage{
setGradientBoostingMachine(
  ntrees = c(100, 300),
  nthread = 20,
  earlyStopRound = 25,
  maxDepth = c(4, 6, 8),
  minChildWeight = 1,
  learnRate = c(0.05, 0.1, 0.3),
  scalePosWeight = 1,
  lambda = 1,
  alpha = 0,
  seed = sample(1e+07, 1)
)
}
\arguments{
\item{ntrees}{The number of trees to build}

\item{nthread}{The number of computer threads to use (how many cores do you have?)}

\item{earlyStopRound}{If the performance does not increase over earlyStopRound number of trees then training stops (this prevents overfitting)}

\item{maxDepth}{Maximum depth of each tree - a large value will lead to slow model training}

\item{minChildWeight}{Minimum sum of of instance weight in a child node - larger values are more conservative}

\item{learnRate}{The boosting learn rate}

\item{scalePosWeight}{Controls weight of positive class in loss - useful for imbalanced classes}

\item{lambda}{L2 regularization on weights - larger is more conservative}

\item{alpha}{L1 regularization on weights - larger is more conservative}

\item{seed}{An option to add a seed when training the final model}
}
\value{
A modelSettings object that can be used to fit the model
}
\description{
Create setting for gradient boosting machine model using gbm_xgboost implementation
}
\examples{
\dontshow{if (rlang::is_installed("xgboost")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
modelGbm <- setGradientBoostingMachine(
  ntrees = c(10, 100), nthread = 20,
  maxDepth = c(4, 6), learnRate = c(0.1, 0.3)
)
\dontshow{\}) # examplesIf}
}
