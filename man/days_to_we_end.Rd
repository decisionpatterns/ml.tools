\name{days_to_we_end}
\alias{days_to_we_end}
\title{Count days to the weekend's end}
\usage{
days_to_we_end(dates, use.holiday = TRUE, type = "NYSE")
}
\arguments{
  \item{dates}{Date objects}

  \item{use.holiday}{Whether to add holidays are the end of
  the weekend.}

  \item{type}{Type of holiday, default: NYSE}
}
\description{
Count the number of days from the date to the end of the
week, optionally accounting for Monday holidays
}
\details{
Considers Monday and Tuesday holidays, but will not
consider Wednesday ...
}

