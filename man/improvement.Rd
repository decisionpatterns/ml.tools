% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imp.R
\name{improvement}
\alias{improvement}
\title{improvement}
\usage{
improvement(obs, ref, decreasing = FALSE)
}
\arguments{
\item{obs}{numeric; observed value of the metric}

\item{ref}{numeric; reference value of the metric, often the "naive" metric}

\item{decreasing}{logical; metrics improves by decreasing instead of
increasing}
}
\description{
Compare metric compared to a reference value
}
\details{
\code{improvement} calculates how much a metric has improved relative to a
reference. \code{decreasing} indicates that the metric gets better as it gets
smaller, for example, with \strong{RMSE}.

It is calculated as:\preformatted{sign * ( obs/ref - 1 )
}
}
\examples{

  obs <- 1.1 
  ref <- 1.0 
  
  imp( obs, ref )
  
  obs <- 0.4
  ref <- 0.5
  improvement( obs, ref, decreasing = TRUE )
 
}
\seealso{
\itemize{
\item \href{kappa()}{kappa}
}
}
