#' Compute pseudo-rsquared for a model
#' 
#' Computes the psuedo-rsquared for a model
#' 
#' @param x the TRUE amount
#' @param xhat estimated amount
#' @export
r2 <- function(x,xhat)
  1 - sum( ( x - xhat )^2 ) / sum( (x -mean(x))^2 ) 
