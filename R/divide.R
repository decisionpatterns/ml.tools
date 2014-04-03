#' Divide data column-wise
#' 
#' Divides data into a list of data objects possibly retaining certain columns
#' in each
#' 
#' @param x a data object
#' 
#' @param by character; names of columns to include in each of the resulting 
#' data objects, default: \code{NULL}.
#' 
#' @param rm character; names of columns to remove, default: \code{NULL}.
#'  
#' @param drop logical; whether to reduce to vectors if it cotains only one 
#' columns
#' 
#' \code{divide} creates a list of data objects, one for each column of \code{x}
#' not in \code{by} or \code{rm}. Each will also contain the \code{by} columns 
#' similar to the way that all \code{by} columns are included when data is split.
#' 
#' If \code{drop} is \code{TRUE} and 
#' 
#' @return 
#' a list of data objects or vectors if \code{by} is \code{NULL} and
#' \code{drop} is \code{TRUE}
#' 
#' @note
#' \code{divide} is mainly useful for situations in which working with 
#' individual columns or groups is easier than working with a wide dataframe. 
#' This often occurs when working with data from foreign sources.
#' 
#' @seealso \code{\link[base]{split}}
#' 
#' @examples 
#'   divide( iris[1:3,], by="Species", )
#'   divide( iris[1:3,], "Petal.Length" )
#'   divide( longley, by="Year" )
#'   divide( longley, by="Year", rm="Employed" )
#'   
#' @export divide

divide <- function( x, by=NULL, rm=NULL, drop=FALSE ) { 

  # `by` and `rm` have to be valid names 
  for( col in c(by,rm) ) 
    if( ! col %in% names(x) ) stop( col, " not found in data.frame." ) 
  
  # SPLIT INTO ALL COLS
  cols <- setdiff( names(x), c(by,rm) )  

  li <- list()
  for( col in cols ) { 
    
    if( ! is.null(by) ) all.col <-  names(x)[ names(x) %in% c(col,by) ]
    
    li[[col]] <- if( is.data.table(x) ) 
      x[ , all.col, drop=drop, with=FALSE  ] else 
      x[ , all.col, drop=drop  ] 
  
  }
    
  return(li)

}
