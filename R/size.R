#' size
#' 
#' Show the size of the object (side-effect only)
#' 
#' @param object, R object
#' 
#' @examples
#'   size(iris)
#'   size(mtcars)
#'   
#' @note TODO:
#'   - make generic 
#'   - move into bigO(?) package 
#'  
#' @export 
#' @import dimensional

size <- function(object) {
  
  size <- list( dim = dim(object), dimensional=sum( dimensional::cardinalities(object) ) )
  
  message( "dim : ", paste( size$dim, collapse=" x " ), " (", prod( size$dim), ")")
  message( "card: ", size$dimensional )
  message( rep( "-", 30 ))
  message( "size: ", sprintf( "%1.3e", size$dim[1] * size$card ) )
  
  invisible(size)
  
}
