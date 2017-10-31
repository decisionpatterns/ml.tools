#' logistic and inverse logistic function
#' 
#' apply the logistic function to \code{x}
#' 
#' @param x numeric. Argument passed to the logistic functions
#' @param y numeric. Argument passed to the inverse logistic function
#' @aliases logistic, logistic_inv, inv.logistic
#' @rdname logistic
#' @export 

  logistic <- function(x) 1/(1+exp(-x))


#' @rdname logistic
#' @export

  logistic_inv <- function(y) -log(1/y-1)

