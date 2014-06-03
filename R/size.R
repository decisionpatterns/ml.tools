#' size
#' 
#' Show the size of the object (side-effect only)
#' 
#' @param object, An R object
#' 
#' @examples
#'   size(iris)
#'   size(mtcars)
#'   
#' @note: TODO:
#'   - make generic 
#'   - move into bigO(?) package 
#'   
#'    
#' @export 

size <- function(object) {
  
  size <- list( dim = dim(object), cardinality=sum( cardinality(object) ) )
  
  message( "dim : ", paste( size$dim, collapse=" x " ), " (", prod( size$dim), ")")
  message( "card: ", size$cardinality )
  
  invisible(size)
  
}
