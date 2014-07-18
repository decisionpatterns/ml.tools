#' Test if a data set has the variables to \code{predict} a model
#' 
#' Makes a comparison betwen \code{names( model$forest$xlevels )} and 
#' \code{names(dat)}
#' @param model randomForest model
#' @param data data to test
#' 
#' @return \code{ invisible}
#' @examples
#'   require(randomForest)
#'   f <- randomForest( Sepal.Length ~ . , iris )
#'   
#'   rf_test_names( f, iris )
#'   rf_test_names( f, iris[ , 1:3] )
#'   rf_test_names( f, mtcars )
#' @export

rf_test_names <- function( model, data ) {
  
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
    
