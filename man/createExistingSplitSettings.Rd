% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSplitting.R
\name{createExistingSplitSettings}
\alias{createExistingSplitSettings}
\title{Create the settings for defining how the plpData are split into
test/validation/train sets using an existing split - good to use for
reproducing results from a different run}
\usage{
createExistingSplitSettings(splitIds)
}
\arguments{
\item{splitIds}{(data.frame) A data frame with rowId and index columns of
type integer/numeric. Index is -1 for test set, positive integer for train
set folds}
}
\value{
An object of class \code{splitSettings}
}
\description{
Create the settings for defining how the plpData are split into
test/validation/train sets using an existing split - good to use for
reproducing results from a different run
}
\examples{
# rowId 1 is in fold 1, rowId 2 is in fold 2, rowId 3 is in the test set
# rowId 4 is in fold 1, rowId 5 is in fold 2
createExistingSplitSettings(splitIds = data.frame(rowId = c(1, 2, 3, 4, 5),
                                                  index = c(1, 2, -1, 1, 2)))
}
