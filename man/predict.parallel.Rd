\name{predict.parallel}
\alias{predict.parallel}
\title{predict.parallel - parallel bulk scoring}
\usage{
  predict.parallel(cl, object, newdata, ...)
}
\arguments{
  \item{cl}{A cluster object from \code{makeCluster}}

  \item{object}{a model object for which prediction is
  desired}

  \item{newdata}{data frame or matrix containing new data}

  \item{...}{Additional arguments affecting the prediction

  Scores a model on a distrubted cluster. The scores are
  distributed using \code{itertools::isplitRows}.  The
  number of chunks is number of clusters.}
}
\value{
  a data.table object with key and associated predictions
}
\description{
  Scores predictive models in parallel.
}
\examples{
\dontrun{ fits <- predict( cl, fit, YX ) }
}
\references{
  http://stackoverflow.com/questions/14756662/parallel-prediction-with-cforest-randomforest-prediction-with-dosnow
}

