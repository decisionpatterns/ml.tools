% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_features.R
\name{check_features}
\alias{check_features}
\title{check_features}
\usage{
check_features(model, data)
}
\arguments{
\item{model}{randomForest model}

\item{data}{data to test}
}
\value{
\code{invisible}
}
\description{
Check if a data set has all the features to \code{predict} a model
}
\details{
Makes a comparison betwen `names( model$forest$xlevels)` and 
`names(dat)`
}
\examples{
  require(randomForest)
  f <- randomForest( Sepal.Length ~ . , iris )
  
  rf_test_names( f, iris )
  rf_test_names( f, iris[ , 1:3] )
  rf_test_names( f, mtcars )
  
}
