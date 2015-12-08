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
#' \code{calibrate} provides a non-parametric, discrete, empirical calibration, 
#' also known as "inverse estimation".  
#' 
#' More specifically it provides a mapping of \code{x} 
#' into the distribution of \code{y}. That is, the rank order of \code{x} is 
#' maintained, but the values are matched to the distribution of \code{y}.
#' 
#' This is useful in predicitve modeling where the model will often correctly 
#' rank order values, but will not appear to be like the modeled distribution. 
#' This may arise from several factors including the modeling or 
#' sampling methodology used.  \code{calibrate} or \code{make.calibrator} can be 
#' used to coerce the resulting scores back into the expected distribution.
#'
#' \code{calibrate} sorts both \code{x} and \code{y} and creates an equivalent
#' pairwise tuple at corresponding points in the distribution. 
#' \code{\link[stats]{approx}} then makes a linear interpolation of \code{x} in 
#' terms of the observed distributions \code{y}.
#'  
#' \code{make.calibrator} works similarly but returns a function that will 
#' subsequently take a vector like \code{x} and return the value as mapped into
#' \code{y}.  This is useful since it can be used to create a calibration 
#' function during model training that can be used subsequently during model
#' scoring.
#'  
#' @return numeric vector; x transformed into basis of y
#' 
#' @seealso 
#'   \code{\link[stats]{approx}} 
#'   \url{http://cran.r-project.org/web/packages/investr} especially the 
#'   calibrate function for another implementation.
#'
#' @examples 
#'   calibrate(1:5, 1:5)   # 1:5
#'   calibrate(1:5, 1:10)  # 1  3  5  7 10
#'   calibrate(5:1, 1:10)  # 10  7  5  3  1
#'   calibrate(1:10, 1:5)  # 1 1 2 2 3 3 4 4 4 5
#'   calibrate(10:1, 1:5)  # 5 4 4 4 3 3 2 2 1 1
#'   
#'   calibrate( rnorm(100,mean=5), rlnorm(2000) )
#'   
#'   make.calibrator(1:5, 1:5) (1:5)  # 1:5
#'   make.calibrator(1:5, 1:10)(1:5)  # 1  3  5  7 10
#'   make.calibrator(5:1, 1:10)(5:1)  # 10  7  5  3  1
#'   make.calibrator(1:10, 1:5)(1:10) # 1 1 2 2 3 3 4 4 4 5
#'   make.calibrator(10:1, 1:5)(10:1) # 5 4 4 4 3 3 2 2 1 1  
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


calibrate <- function( x, y, method="constant", rule=2,  ... ){

  x. <- sort(x)
  y  <- sort(y)
  
  # Make both vectors the length of the shorter 
  x. <- x.[ seq( 1, length(x), length.out = min( length(x), length(y) ) ) ]
  y  <-  y[ seq( 1, length(y), length.out = min( length(x), length(y) ) ) ]

  
  
  return(
    approx( x., y, xout=x, method=method, rule=rule, ... )$y
  )
  
}


#' @rdname calibrate 
#' @aliases calibratefun make.calibration make.calibrator
#' @export

make.calibrator <- 
  make.calibration <- calibratefun <- function( x, y, method="constant", rule=2,  ...){
  
  x <- sort(x)
  y <- sort(y)
  
  x <-  x[ seq( 1, length(x), length.out = min( length(x),length(y) ) ) ]
  y <-  y[ seq( 1, length(y), length.out = min( length(x),length(y) ) ) ]
  
  return( 
    approxfun( x, y,  method=method, rule=rule, ... )
  )
  
}

