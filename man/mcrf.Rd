\name{mcrf}
\alias{mcrf}
\title{Train Random Forest Model using Multicore}
\usage{
  mcrf(..., ntree = 500, combine = TRUE,
    ncores = parallel::detectCores() - 1)
}
\arguments{
  \item{...}{All parameters passed to \code{randomForest}}

  \item{ntree}{Number of trees to build}

  \item{combine}{(logical). Whether to combine the results
  using randomForest::combine}

  \item{ncores}{Number of cores to use}
}
\value{
  A random forest object.
}
\description{
  Trains a Random Forest Model using Multicore and Foreach
}
\details{
  Trains a Random Forest model using multicores.
}
\examples{
fit <- mcrf( Species ~ ., data=iris, ntree=13 )
}
\seealso{
  caret, foreach
}
\keyword{model}

