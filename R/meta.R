#' Create a list of values associated with data
#' 
#' Creates a list based on a data. The list contains the values found in the 
#' categorical variables of the data. The 
#' 
#' @param data a data object
#' 
#' @seealso 
#'    \code{\link{add_meta_values}}
#'    
#' @importFrom catcont is_cat
#' @export

meta_values <- function(data) {
  
  values <- list()  
  for( nm in names(data) ) 
    if( is_cat( data[[nm]] ) ) { 
      values[[nm]] <- as.character( sort( unique(data[[nm]]) ) )  
    }
  
  return( values )
  
}


#' Add 'values' attribute to data
#' 
#' Add metadata 'values' attribute that containsa list of values associated
#' with categorical variables associated with a data frame
#' 
#' @param data a data object
#' 
#' @seealso \code{\link{meta_values}}
#' @export 

add_meta_values <- function(data) {
  
  values <- meta_values( data )
  attr(data, "values") <- values 
  
  # Add 'prototype' attr to keep track of frame 
  attr( data, "prototype" ) <- data[0,]
  
  return(data) 
}


#' Create prototype for data
#' 
#' Create a prototype for the data.  A prototype is a zero-row copy that retains
#' all attributes, columns, etc. just without data. 
#' @param data a data object
#' @seealso \code{\link{add_meta_prototype}}
#' @export

meta_prototype <- function(data) return( data[0,] )


#' Add 'prototype' attribute to data
#' 
#' Add \code{prototype} attribute to data.  The \code{prototype} attribute is 
#' a zero-row data.frame
#' @param data a data object
#' @seealso \code{\link{meta_prototype}}
#' @export

add_meta_prototype <- function(data) {
  attr( data, "prototype" ) <- data[0,]
  return(data)
}

