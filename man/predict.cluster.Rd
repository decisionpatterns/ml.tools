\name{predict.cluster}
\alias{predict.cluster}
\title{predict.clusrers - parallel bulk scoring}
\usage{
  predict.cluster(cl, object, newdata, ...)
}
\arguments{
  \item{cl}{A cluster object}

  \item{object}{a model object for which prediction is
  desired}

  \item{newdata}{data frame or matrix containing new data}

  \item{...}{Additional arguments affecting the prediction

  Scores a model on a distrubted cluster}
}
\value{
  a data.table object with key and associated predictions
}
\description{
  Scores predictive models in parallel.
}
\examples{
\dontrun{fits <- predict( cl, fit, YX )}
}
\references{
  http://stackoverflow.com/questions/14756662/parallel-prediction-with-cforest-randomforest-prediction-with-dosnow
}

