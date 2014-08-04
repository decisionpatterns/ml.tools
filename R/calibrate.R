#' Calibrate 
#' 
#' Calibrates a set of values to an empirical distribution.   
#' 
#' @param x numeric; values to be calibrated
#'  
#' @param y numeric; an empirical distributions to serve as basis for the 
#' calibration
#' 
#' @param method character; argument passed to \code{\link[stats]{approx}}. 
#' Here, though, the default is \code{constant} to better match with empirical 
#' distributions that may not have certain values represented.
#' 
#' @param rule integer; argument passed to \code{\link[stats]{approx}}. Here the 
#' default is 2, i.e. values outside the range of \code{y} are interpreted as 
#' the closest extreme.
#' 
#' @param ... additional arguments passed to \code{\link[stats]{approx}}
#'
#' \code{calibrate} provides a non=parametrics mapping of values to closely 
#' mimick an empirical distribution.  This is useful in predicitve modeling 
#' where the model will correctly rank order values, but either approximate the  
#' true distributions or provide values that are usable. 
#'
#' The function sorts the distributions then ensures they're the same size
#' then uses \code{\link[stats]{approx}} to linear interpolate \code{x} in terms
#' of the observed distributions \code{y}.
#'  
#' @return numeric vector; x transformed into basis of y
#' 
#' @seealso 
#'   \code{\link[stats]{approx}} 
#' 
#' @examples 
#'   calibrate(1:5, 1:5)   # 1:5
#'   calibrate(1:5, 1:10)  # 1  3  5  7 10
#'   calibrate(5:1, 1:10)  # 10  7  5  3  1
#'   calibrate(1:10, 1:5)  # 1 1 2 2 3 3 4 4 4 5
#'   calibrate(10:1, 1:5)  # 5 4 4 4 3 3 2 2 1 1
#'   
#'   calibrate( rnorm(1000,mean=5), rlnorm(1000) )
#'   
#'   \dontrun{
#'   
#'     x <- rnorm(1000,mean=5)  # e.g. model scores
#'     y <- rlnorm(1000)        # original distributions
#'      
#'     plot.univariates( x, calibrate( x, y ), y )
#'   }
#'   
#' @aliases calibrate
#' @rdname calibrate
#' @export


calibrate <- function(x, y, method="constant", rule=2,  ... ){

  x. = sort(x)
  y. = sort(y)
  
  # Make both vectors the length of the shorter 
  x. = x.[ seq( 1, length(x), length.out = min( length(x), length(y)) ) ]
  y. = y.[ seq( 1, length(y), length.out = min( length(x), length(y)) ) ]

  calibrated <- approx( x., y., xout=x, method=method, rule=rule, ...  )
  
  return (calibrated$y)
  
}

