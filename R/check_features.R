#' check_features 
#' 
#' Check if a data set has all the features to \code{predict} a model
#' 
#' @param data data to test
#' @param model randomForest model
#' 
#' @details 
#' 
#' Makes a comparison betwen `names( model$forest$xlevels)` and 
#' `names(dat)`
#' 
#' @return \code{invisible} 
#' 
#' @examples
#'   require(randomForest)
#'   f <- randomForest( Sepal.Length ~ . , iris )
#'   
#'   rf_test_names( f, iris )
#'   rf_test_names( f, iris[ , 1:3] )
#'   rf_test_names( f, mtcars )
#'   
#' @export

check_features <- function( model, data ) UseMethod('check_features')


# rf_test_names <- function( model, data ) {
check_features.randomForest <- function( model, data ) { 

  sd <- setdiff( names( model$forest$xlevels ), names(data) )
  
  if( length(sd) > 0 ) { 
    
    warning( "Names that are not found in '", deparse(substitute(dat) ), "'\n  "
      , paste( sd, collapse=", " ) 
    )
    return( invisible(1) )
    
  } else { 
    return( invisible(0))
  }
  
}

check_features.train <- function( model, data ) 
  stop("check_features is not implemented for object of type train.")
