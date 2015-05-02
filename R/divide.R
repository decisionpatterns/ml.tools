#' Divide data column-wise
#' 
#' Divides data into a list of data objects possibly retaining certain columns
#' in each resulting table as keys
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
#' @param secondary.key logical; whether to set the 
#' 
#' @param na.action function; function applied to resulting object to handle NA
#' values. Default: \code{\link[stats]{na.pass}}
#' 
#' @param ... additional arguments passed to specific elements
#' 
#' \code{divide} creates a list of data objects, one for each column of \code{x}
#' that are not in \code{by} or \code{rm}. Each will also contain the \code{by} columns 
#' similar to the way that all \code{by} columns are included when data is split.
#' If \code{x} is a data.table, 
#' 
#' 
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
#'   # data.frame method
#'   data(iris)
#'   divide( iris )
#'   divide( iris, by='Species', rm='Petal.Length' )
#'   divide( iris, drop=TRUE )
#'   divide( longley, by='Year' )
#'   divide( longley, by='Year', rm='Employed' )
#'   
#'   # data.table method
#'   data(iris)
#'   setDT(iris)
#'   divide(iris)
#'   divide(iris, "Species" )
#'   divide(iris, "Species", secondary.key=TRUE )
#'   
#' @export divide

divide <- function( x, by, ... ) UseMethod( 'divide' )


#' @rdname divide
#' @aliases divide,data.frame-method
#' @export
# @method divide data.frame
divide.data.frame <- function( x, by=NULL, rm=NULL, drop=FALSE, na.action=na.pass ) { 

  # `by` and `rm` have to be valid names 
  for( col in c(by,rm) ) 
    if( ! col %in% names(x) ) stop( col, " not found in data.frame." ) 
  
  # SPLIT INTO ALL CO LS
  cols <- setdiff( names(x), c(by,rm) )  

  li <- list()
  for( col in cols ) { 
    
    all.col <- if( is.null(by) ) col else c(by,col) 
    
    li[[col]] <- na.pass( x[ , all.col, drop=drop  ] )
    
  }
    
  return(li)

}

#' @rdname divide
#' @method divide data.table
#' @aliases divide,data.table-method
#' @export


divide.data.table <- function( 
  x, by=NULL, rm=NULL, drop=FALSE, na.action=na.pass, secondary.key=FALSE 
) { 

  # `by` and `rm` have to be valid names 
  for( col in c(by,rm) ) 
    if( ! col %in% names(x) ) stop( col, " not found in data.frame." ) 
  
  # SPLIT INTO ALL COLS
  cols <- setdiff( names(x), c(by,rm) )  

  li <- list()
  for( col in cols ) { 
    
    all.col <- if( is.null(by) ) col else c(by,col) 
    
    li[[col]] <- x[ , all.col, drop=drop, with=FALSE ] 
   
    if( is.function(na.action) ) li[[col]] <- na.action( li[[col]] )
    
    setkeyv( li[[col]], if( secondary.key ) col else by )
    
  }
    
  return(li)

}
