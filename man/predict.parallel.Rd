\name{predict.parallel}
\alias{predict.parallel}
\title{predict.parallel - parallel bulk scoring}
\usage{
  predict.parallel(object, newdata, ...)
}
\arguments{
  \item{object}{a model object for which prediction is
  desired}

  \item{newdata}{data frame or matrix containing new data}

  \item{...}{Additional arguments passed to the predict
  function

  Scores a model on a distrubted cluster using
  \code{foreach} and \code{itertools::isplitRows} which
  chunks the data into one chunck for each node.}
}
\value{
  vector of associated predictions
}
\description{
  Drop in replacement for \code{predict} that scores
  predictive models in parallel.
}
\examples{
require(randomForest)
  data(iris)
  fit  <- randomForest(Species ~ ., iris)
  predict( fit, iris )
  predict.parallel( fit, iris )
}
\references{
  http://stackoverflow.com/questions/14756662/parallel-prediction-with-cforest-randomforest-prediction-with-dosnow
}
\seealso{
  \code{\link[foreach]{foreach}},
  \code{\link[itertools]{isplitRows}}
}

