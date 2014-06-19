#' features - a data.table of features 
#' 
#' Create a S3 feature data.table by blessing into a class an optionally
#' setting the key for the data set
#' 
#' @param data to become features
#' @param key character; names of columns to use as the key, \code{NULL} the 
#' default will use either existing keys or \code{ getOption('feature.key') }
#' 
#' The function works by converting the data to a data.table and attempting to
#' set a key on the data. The key will be determined by the explicit \code{key}
#' argument to this function, any existing key or the global option, 
#' \code{feature.key}. If no key is defined a warning is issued.
#'
#' @return 
#'   a data.table that includes class 'features' with an optional key set.
#'   
#' @examples
#'   features( iris, key='Species' ) 
#'   features( mtcars, "cyl" )     
#' @export   

features <- function(data, key=NULL ) {
  
  setDT(data)  
  class(data) <- c( 'features', class(data) )
  
  # KEY DEFINED BY EXPLICITLY IN FUNCTION CALL
  if( ! is.null(key) ) { 
    setkeyv( data, key )
    return(data)
  } 
  
  # KEY ALREADY EXISTS ON DATA
  if( ! is.null(key(data) ) ) return(data)
  
  # GLOBAL OPTION: feature.key
  if( getOption('feature.key') %in% names(data) ) {
    setkeyv( data, getOption('feature.key') )
    return(data)
  }
  
  # NO KEY
  warning( "Feature data does not have a key linking it to an entity.")
  
  return(data)

} 
