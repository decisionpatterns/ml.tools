\name{divide}
\alias{divide}
\title{Divide data column-wise}
\usage{
  divide(x, by = NULL, rm = NULL, drop = FALSE)
}
\arguments{
  \item{x}{a data object}

  \item{by}{character; names of columns to include in each
  of the resulting data objects, default: \code{NULL}.}

  \item{rm}{character; names of columns to remove, default:
  \code{NULL}.}

  \item{drop}{logical; whether to reduce to vectors if it
  cotains only one columns

  \code{divide} creates a list of data objects, one for
  each column of \code{x} not in \code{by} or \code{rm}.
  Each will also contain the \code{by} columns similar to
  the way that all \code{by} columns are included when data
  is split.

  If \code{drop} is \code{TRUE} and}
}
\value{
  a list of data objects or vectors if \code{by} is
  \code{NULL} and \code{drop} is \code{TRUE}
}
\description{
  Divides data into a list of data objects possibly
  retaining certain columns in each
}
\note{
  \code{divide} is mainly useful for situations in which
  working with individual columns or groups is easier than
  working with a wide dataframe. This often occurs when
  working with data from foreign sources.
}
\examples{
divide( iris[1:3,], by="Species", )
  divide( iris[1:3,], "Petal.Length" )
  divide( longley, by="Year" )
  divide( longley, by="Year", rm="Employed" )
}
\seealso{
  \code{\link[base]{split}}
}

