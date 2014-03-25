\name{divide}
\alias{divide}
\title{Divide data column-wise into separate data}
\usage{
divide(x, cols = NULL, keep = NULL, rm = NULL, drop = FALSE)
}
\arguments{
  \item{x}{a data object}

  \item{all}{character; names of columns included in each
  divided frame}

  \item{keep}{character; names of columns to include in
  each}

  \item{rm}{character; names of columns to remove}

  \item{drop}{logical; whether to reduce to vectors if it
  cotains only one columns

  \code{divide} creates data.frames comprised of the}
}
\value{
a list of vectors or data objects
}
\description{
Divides data into multiple objects such retaining indices
}
\examples{
divide( iris[1:3,], keep="Species", )
  divide( iris[1:3,], rm=c("Sepal.Length", "Petal.Length") )
  divide( longley, keep="Year" )
}
\seealso{
\code{\link[base]{split}}
}

