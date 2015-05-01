#' Automatically set data.table keys
#' 
#' Atomatically sets the keys of a data.table based on \code{options}.   
#' 
#' @param dt data.table
#' @param keys character; the values to set for keys if present in \code{dt}. 
#' 
#' \code{autokey} sets the keys for a data.table. The default is to look at the
#' \code{getOption('autokey')} for the list available keys. It is equivalent
#' to setting the \code{cols} argument of \code{setkeyv} equivalent to:
#' 
#' \code{ intersect( names(x), getOption('autokey') ) }
#' 
#' Keys are used by \code{\link{stitch}} for determining how various data 
#' vectors are related.
#'
#' @seealso 
#'   \code{\link[data.table]{setkeyv}} in the \code{data.table} package.
#'    
#' @examples
#'   options( autokey='Species' )
#'   data(iris)
#'   setDT(iris)
#'   autokey(iris)   
#'   
#' @import data.table   
#' @export

  autokey <- function( x, keys=getOption('autokey') ) {
    
    if( data.table::is.data.table(x) )
      data.table::setkeyv( x, intersect( names(x), keys ) )
  
  }

#' Set the autokeys
#' 
#' Set the column for autokeys
#' 
#' @param x character of column names to be automatically set as keys
#' 
#' This is nothing more than a wrapper for options(autokey=x)
#' 
#' @rdname autokey
#' @export 
  set_autokeys <- function(x) options( autokey=x ) 

#' Get the autokeys
#' 
#' Get the vector of autokeys
#' 
#' Simple wrapper of \code{getOption('autokey')}
#' @rdname autokey
#' @export
  autokeys <- function() getOption( 'autokey')
  