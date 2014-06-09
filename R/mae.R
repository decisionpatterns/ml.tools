#' Compute the median absolute error (MAE) of an estimate. 
#' 
#' Computes the median absolute error of en estimate or model.
#' 
#' The operation is commutative; the order of arguments does not matter. If 
#' either \code{x} or \code{xhat} have missing values, then that tuple is 
#' omitted from the calculations similar to \code{na.omit}. 
#' 
#' @param x numeric. The actual/true values. 
#' @param xhat numeric. The estimated/predicted values.
#' @return numeric. The calculated median absolute error.
#' @export
mae <- function( x, xhat ) { 

   # Purge na values 
   if( length(x) != length(xhat) ) 
     warning( immediate.=TRUE, "** Observed and estimate are of differnt lengths. ** ") 
   wh <- ! ( is.na(x) || is.na(xhat) ) 
   # x[wh] - xhat[wh]  
   mean( abs( x[wh] - xhat[wh] ), na.rm=TRUE ) 
   
}
