% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Imputation.R
\name{simpleImpute}
\alias{simpleImpute}
\title{Simple Imputation}
\usage{
simpleImpute(trainData, featureEngineeringSettings, done = FALSE)
}
\arguments{
\item{trainData}{The data to be imputed}

\item{featureEngineeringSettings}{The settings for the imputation}

\item{done}{Whether the imputation has already been done (bool)}
}
\value{
The imputed data
}
\description{
This function does single imputation with the mean or median
}
\keyword{internal}
