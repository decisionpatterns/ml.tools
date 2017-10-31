#' softmax
#' 
#' calculate the softmax of values
#' 
#' @param x 
#' 
#' @seealso 
#'  - [logistic](logistic())
#' @examples 
#' 
#' x <- c(1, 2, 3, 4, 1, 2, 3)
#' softmax(x)
#' 
#' @export 

softmax <- function(x) exp(x)/sum(exp(x))
