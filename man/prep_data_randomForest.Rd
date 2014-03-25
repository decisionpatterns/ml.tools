\name{prep_data_randomForest}
\alias{prep_data_randomForest}
\title{randomForest requires
* factors and not strings.
* factors <= 32 levels
* no missing values
* also saves 'values'
* Inf, -Inf not allowed (at least not in caret )
* predict additionally requires that there are no new levels in
  \code{newdata}. Each level does not have to be represented, but there needs
  all levels must be represent in the \code{levels} attribute.}
\usage{
  prep_data_randomForest(data, ..., max.levels = 32,
    preserve = character())
}
\arguments{
  \item{data}{data.frame.}

  \item{preserve}{(character) columns not to prepare.}

  \item{...}{arguments passed to other functions}
}
\description{
  randomForest requires * factors and not strings. *
  factors <= 32 levels * no missing values * also saves
  'values' * Inf, -Inf not allowed (at least not in caret )
  * predict additionally requires that there are no new
  levels in \code{newdata}. Each level does not have to be
  represented, but there needs all levels must be represent
  in the \code{levels} attribute.
}
\examples{
# -tk
}

