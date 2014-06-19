require( itertools )

#' ifeatures - iterator over features 
#' 
#' iterator for iterating of a data.table of features
#' 
#' @param x data.table containing a key and measures 
#' @param chunkSize number of measures to include along with 
#'  
#' A feature is defined as a measurement associated with a particular entity. 
#' In this case the entity is defined by the key of the data.table. ifeatures 
#' iterators over the non-key columns   
#' 
#' @seealso
#'   \code{\link{divide}}
#'   \code{\link[itertools]{isplitVector}}
#'   \code{\link[itertools]{ihasNext}}
#'   
#' @examples
#'   data(iris)
#'   setDT(itis)
#'   setkey(iris, Species)
#'   
#'   it <- ifeatures(iris)
#'   while( hasNext(it) ) 
#'     message( paste( dim(nextElem(it)), collapse=" x " ) ) 
#'                 
#' @export  

ifeatures <- function(x, chunkSize=1, ... ) {
 
  keys <- key(x)
  if( is.null(keys) ) stop( "No key identified for use as feature iterator")
  if( length(keys) == ncol(x) ) stop( "There are only key/id variables")
      
  # it <- idiv( ncol(x) - length(keys), chunkSize=1, ... ) # for each column 
  it <- ihasNext( isplitVector( setdiff( names(x), keys ), chunkSize=1, ... ) )
  
  nextEl <- function() {   
    x[ , c(keys, nextElem(it)), with=FALSE ]
  }
  
  hasNx <- function() { 
    hasNext(it)
  }
  
  object <- list(nextElem = nextEl, hasNext=hasNx )
  class(object) <- c("ihasNext", "abstractiter", "iter")
  object
}  


