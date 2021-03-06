% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unimetrics.R
\name{unimetrics}
\alias{unimetrics}
\title{unimetrics / allmetrics}
\usage{
unimetrics(
  data,
  formula,
  metric = ifelse(is_cat(data[[lhs]]), "Accuracy", "RMSE"),
  family = ifelse(is_cat(data[[lhs]]), binomial, gaussian)
)
}
\arguments{
\item{metric}{string; See [train](caret::train())}
}
\description{
Measure variable performance singular or as
}
\examples{

data(iris)




}
