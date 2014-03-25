#' Calculate model performance
#' 
#' Calculates the performance of a fitted model.
#' 
#' @param model a model objecy. The model to be evaluated.
#' @param newdata Data on which to evaluate the model. Usually the test set. 
#' @param fun A function that accepts (at least) two arguments the fitted and 
#' the actual value 
#' @param ... Arguments passed to \code{fun}
#' @return The result of \code{fun} applied to the actual and fitted value
#' @seealso \code{mae}, \code{rmse}
#' @author Christopher Brown
#' @examples
#'   fit <- lm( Sepal.Length ~ ., iris )
#'   eval_model( fit, iris )
#'   eval_model( fit, iris, rmse )
#'   eval_model( fit, iris, mae )
#' @note Does not allow for post model evaluation
#' @export  
eval_model <- function( model, newdata, fun, ... ) 
  UseMethod( "eval_model")



#' @method eval_model default
#' @S3method eval_model default 
eval_model.default <- function( model, newdata, fun=rmse, ... ) { 
  
  y <- as.character( lhs( formula(model) ) )
  
  return( 
    fun( newdata[[y]], predict( model, newdata ) ) 
  )
  
}


