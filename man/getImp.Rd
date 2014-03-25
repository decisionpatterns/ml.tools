\name{getImp}
\alias{getImp}
\title{getImp}
\usage{
getImp(model, n = 50)
}
\arguments{
  \item{model}{A rf model or at least something that has an
  \code{importance}}

  \item{n}{number of features to extract}
}
\value{
factor
}
\description{
Get most important features from an RF model in order of
importance
}

