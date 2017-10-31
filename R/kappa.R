#' Cohen's Kappa, 
#'
#' Calculate Kohen's Kappa, an accuracy adjust for expectations 
#' 
#' @param obs numeric; observed accuracy 
#' @param ref numeric; reference/expected accuracy 
#' 
#' @details 
#' 
#' Like [improvement](improvement()), `kappa` measures the improvement in a metric 
#' (typically: *accuracy*) but normalizes the improvement by the best possible 
#' value, e.g. `accuracy == 1`. 
#' ` 
#' Cohen's Kapps is calculated as:
#' 
#'      ( obs - ref ) / ( 1 - ref )
#' 
#' It can be thought of as how much improvement has been achieved versus how much 
#' can be achieved. Likewise `1-kappa` can be thought of as home much additional 
#' improvements can be made. Cohen's Kappa assumes the best model is 1. For this 
#' reason `kappa1` is an alias for `kappa`.
#' 
#' 
#' `kappa0` is similar to `kappa1` but assumes that best possible score is 0 and
#' not 1. This would be a  
#'    `
#' 
#' @seealso 
#' 
#'   [improvement](improvement()) 
#' 
#' @examples 
#' 
#'  ref <- 0.95    # naive accuracy
#'  obs <- 0.98    # model accuracy
#'
#'  kappa(0.98,0.95)
#'    
#'  kappa0(0.40, 0.50)    
#'    
#' @md
#' @export

kappa <- function( obs, ref ) { 

  if( ! is.numeric(obs) ) stop("'obs' must be numeric")
  if( ! is.numeric(ref) ) stop("'ref' must be numeric")  
  
  ( obs - ref ) / ( 1 - ref )
}


#' @rdname kappa
#' @export 
kappa1 <- kappa


#' @rdname kappa
#' @export 

kappa0 <- function( obs, ref ) { 

  if( ! is.numeric(obs) ) stop("'obs' must be numeric")
  if( ! is.numeric(ref) ) stop("'ref' must be numeric")  
  
  ( obs - ref ) / ( 0 - ref )
  
}
