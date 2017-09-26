#' imp 
#' 
#' Report the improvement in a linear metric 
#'
#' @param metric numeric; value of the metric  
#' @param ref numeric; reference value of the metric, often the "naive" metric
#' @param decreasing logical; metrics improves by decreasing instead of 
#'        increasing 
#' 
#' @details
#' 
#' Calculated as:
#' 
#'     sign * ( metric/ref - 1 )
#' 
#' @seealso 
#' @examples 
#' 
#'   metric <- 1.1 
#'   ref <- 1.0 
#'   
#'   imp( metric, ref )
#'   imp( metric, ref, decreasing = TRUE )
#'   
#' @export 

imp <- function( metric, ref, decreasing = FALSE ) {
  
  sign <- if( decreasing ) -1 else +1 
  
  if( ! is.numeric(metric) ) stop("'metric' must be numeric")
  if( ! is.numeric(ref)    ) stop("'ref' must be numeric")
  
  return( sign * ( metric / ref - 1 )  )
  
}


#' @export 
#' @rdname imp

importance <- imp