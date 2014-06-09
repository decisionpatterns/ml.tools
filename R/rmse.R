#' Compute the root mean squared error
#' 
#' Computes the root mean square error (rmse) of en estimate or model.
#' @param x numeric. The true/actual values. 
#' @param xhat numeric. The predicted/estimated values.
#' @details 
#' The operation is commutative so the order of operations does not matter. If 
#' either \code{x} or \code{xhat} have missing values, then that tuple is 
#' omitted from the calculations similar to \code{na.omit}. 
#' @export
rmse <- function( x, xhat ) {
  
  # Purge na values 
  if( length(x) != length(xhat) ) 
    warning( immediate.=TRUE, "** Observed and estimate are of differnt lengths. ** ") 
  wh <- ! ( is.na(x) || is.na(xhat) ) 
  
  sqrt( mean( (x[wh] - xhat[wh])^2 ) )

}