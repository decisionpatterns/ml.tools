require( itertools )

#' ifeatures - iterator over features 
#' 
#' iterator for iterating of a data.table of features
#' 
#' @param x data.table containing a key and measures 
#' @param chunkSize number of measures to include along with 
#' @param ... additional parameters passed to \code{ihasNext}
#'  
#' A feature is defined as a measurement associated with a particular entity. 
#' The entities are defined by the key columns.  The measurements are non-key
#' columns.  
#'
#' nextElem iterates over the measurement (non-key) column and provides a
#' data.table with the keys of x and one measurement.  
#' 
#' The idea is that the measurements can be independently operated on.
#'        
#' @return 
#'   an iterator
#' 
#' @seealso
#'   \code{\link{divide}}
#'   \code{\link[itertools]{isplitVector}}
#'   \code{\link[itertools]{ihasNext}}
#'   
#' @examples
#'   data(iris)
#'   setDT(iris)
#'   setkey(iris, Species)
#'   
#'   it <- ifeatures(iris)
#'   el1 <- nextElem(it)
#'   key(el1)
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
    x. <- x[ , c(keys, nextElem(it)), with=FALSE ]
    setkeyv(x.,keys)
    x.
  }
  
  hasNx <- function() { 
    hasNext(it)
  }
  
  object <- list(nextElem = nextEl, hasNext=hasNx )
  class(object) <- c("ihasNext", "abstractiter", "iter")
  object
}  


