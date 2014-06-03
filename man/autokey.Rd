\name{autokey}
\alias{autokey}
\alias{autokeys}
\alias{set_autokeys}
\title{Automatically set data.table keys}
\usage{
  autokey(dt, keys = getOption("autokey"))

  set_autokeys(x)

  autokeys()
}
\arguments{
  \item{dt}{data.table}

  \item{keys}{character; the values to set for keys if
  present in \code{dt}.

  \code{autokey} sets the keys for a data.table. The
  default is to look at the \code{getOption('autokey')} for
  the list available keys.

  Keys are used by \code{\link{stitch}} for determining how
  various data vectors are related.}

  \item{x}{character of column names to be automatically
  set as keys

  This is nothing more than a wrapper for
  options(autokey=x)}
}
\description{
  Atomatically sets the keys of a data.table based on
  \code{options}

  Set the column for autokeys

  Get the vector of autokeys
}
\details{
  Simple wrapper of \code{getOption( 'autokey')}
}
\examples{
options( autokey='Species' )
  data(iris)
  setDT(iris)
  autokey(iris)
}

