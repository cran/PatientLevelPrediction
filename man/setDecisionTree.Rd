% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SklearnClassifierSettings.R
\name{setDecisionTree}
\alias{setDecisionTree}
\title{Create setting for the scikit-learn DecisionTree with python}
\usage{
setDecisionTree(
  criterion = list("gini"),
  splitter = list("best"),
  maxDepth = list(as.integer(4), as.integer(10), NULL),
  minSamplesSplit = list(2, 10),
  minSamplesLeaf = list(10, 50),
  minWeightFractionLeaf = list(0),
  maxFeatures = list(100, "sqrt", NULL),
  maxLeafNodes = list(NULL),
  minImpurityDecrease = list(10^-7),
  classWeight = list(NULL),
  seed = sample(1e+06, 1)
)
}
\arguments{
\item{criterion}{The function to measure the quality of a split. Supported criteria are “gini” for the Gini impurity and “entropy” for the information gain.}

\item{splitter}{The strategy used to choose the split at each node. Supported strategies are “best” to choose the best split and “random” to choose the best random split.}

\item{maxDepth}{(list) The maximum depth of the tree. If NULL, then nodes are expanded until all leaves are pure or until all leaves contain less than min_samples_split samples.}

\item{minSamplesSplit}{The minimum number of samples required to split an internal node}

\item{minSamplesLeaf}{The minimum number of samples required to be at a leaf node. A split point at any depth will only be considered if it leaves at least minSamplesLeaf training samples in each of the left and right branches. This may have the effect of smoothing the model, especially in regression.}

\item{minWeightFractionLeaf}{The minimum weighted fraction of the sum total of weights (of all the input samples) required to be at a leaf node. Samples have equal weight when sampleWeight is not provided.}

\item{maxFeatures}{(list) The number of features to consider when looking for the best split (int/'sqrt'/NULL)}

\item{maxLeafNodes}{(list) Grow a tree with max_leaf_nodes in best-first fashion. Best nodes are defined as relative reduction in impurity. If None then unlimited number of leaf nodes. (int/NULL)}

\item{minImpurityDecrease}{Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf.}

\item{classWeight}{(list) Weights associated with classes 'balance' or NULL}

\item{seed}{The random state seed}
}
\value{
a modelSettings object
}
\description{
Create setting for the scikit-learn DecisionTree with python
}
\examples{
\dontshow{ # dontrun reason: requires python's scikit-learn, checkSklearn() will error without it }
\dontrun{ 
model <- setDecisionTree(criterion = list("gini"),
                         maxDepth = list(4),
                         minSamplesSplit = list(2),
                         minSamplesLeaf = list(10),
                         seed = 42)
}
}
