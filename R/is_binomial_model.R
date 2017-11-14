#' is_binomial_model
#' 
#' @export

is_binomial_model <- function(x) UseMethod('is_binomial_model')

#' @rdname is_binomial_model
#' @export
is_binomial_model.caret <- function(x) { 
  x$modelType == "Classification" && 
  ( x$levels %>% length %>% equals(2) )
}
