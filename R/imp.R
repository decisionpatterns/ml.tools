#' imp[rovement]
#' 
#' Report the improvement of an observed value of a metric compared to a 
#' reference value 
#'
#' @param obs numeric; observed value of the metric  
#' @param ref numeric; reference value of the metric, often the "naive" metric
#' @param decreasing logical; metrics improves by decreasing instead of 
#'        increasing 
#' 
#' @details
#' 
#' `improvement` calculates how much a metric has improved relative to a 
#' reference value. This is similar to [Cohen's Kappa](kappa()).  
#' 
#' It is calculated as:
#' 
#'     sign * ( obs/ref - 1 )
#' 
#' @seealso
#' 
#'  - [kappa](kappa())
#'     
#' @examples 
#' 
#'   obs <- 1.1 
#'   ref <- 1.0 
#'   
#'   imp( obs, ref )
#'   
#'   obs <- 0.4
#'   ref <- 0.5
#'   imp( obs, ref, decreasing = TRUE )
#'  
#' @md  
#' @export 

imp <- function( obs, ref, decreasing = FALSE ) {
  
  sign <- if( decreasing ) -1 else +1 
  
  if( ! is.numeric(obs) ) stop("'obs' must be numeric")
  if( ! is.numeric(ref) ) stop("'ref' must be numeric")
  
  return( sign * ( obs / ref - 1 )  )
  
}


#' @export 
#' @rdname imp

importance <- imp