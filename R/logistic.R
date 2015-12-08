#' logistic and inverse logistic function
#' 
#' apply the logistic function to \code{x}
#' 
#' @param x numeric. Argument passed to the logistic functions
#' @param y numeric. Argument passed to the inverse logistic function
#' @aliases logistic, logistic_inv, inv.logistic
#' @export 
#' @rdname logistic

  logistic <- function(x) 1/(1+exp(-x))

#' @export
#' @rdname logistic

  logistic_inv <- function(y) -log(1/y-1)

