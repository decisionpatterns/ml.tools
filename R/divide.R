#' Divide data column-wise into separate data
#' 
#' Divides data into multiple objects such retaining indices 
#' @param x a data object
#' @param all character; names of columns included in each divided frame
#' @param keep character; names of columns to include in each 
#' @param rm character; names of columns to remove
#' @param drop logical; whether to reduce to vectors if it cotains only one columns
#' 
#' \code{divide} creates data.frames comprised of the     
#' 
#' @return a list of vectors or data objects
#' 
#' @seealso \code{\link[base]{split}}
#' 
#' @examples 
#'   divide( iris[1:3,], keep="Species", )
#'   divide( iris[1:3,], rm=c("Sepal.Length", "Petal.Length") )
#'   divide( longley, keep="Year" )
#'   
#' @export divide

divide <- function( x, cols=NULL, keep=NULL, rm=NULL, drop=FALSE ) { 

  if( is.null(cols) && is.null(rm) ) 
    cols <- setdiff( names(x), keep ) 
  
  if( is.null(cols) && ! is.null(rm) ) 
    cols <- setdiff( names(x), c( rm, keep ) ) 

  li <- list()
  for( col in unique(cols) ) { 
  
    if( ! is.null(keep) ) all.col <-  names(x)[ names(x) %in% c(col,keep) ]
    li[[col]] <- x[ , all.col, drop=drop, with=FALSE ] 
  
  }
    
  return(li)

}
