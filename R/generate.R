#' generate features
#' 
#' generate features
#'
#' @param x (character) name of column to convert 
#' @param data (data.frame or data.table) 
#' @param weight numeric; optional weighting 
#' @param sep character; character separator 
#' @param fun function;
#'   
#' @details 
#' generates features based on levels of a categorical
#' variable.
#' 
#' @return a set of features ...
#' 
#' @seealso dummies
#' 
#' @examples
#'   data(iris)
#'   #' generate( data=iris, x="Species" )  
#'
#' @note
#'  - This is supposed to work by splitting according to 
#'  - TODO: make this more like \code{\link{reshape}}
#'  
#' @export

generate <- function(x, data, weight=NULL, sep=".", fun=sum ) {
  
  dumb <- dummy( x, data, sep=sep  )
  
  # COUNTS
  dumb <- if( is.null(weight) ) dumb else dumb*eval(weight,data) 
  setDT(dumb)
 
  dumb[ , parent_id := data[[x]] ]
  dumb <- dumb[ , lapply(.SD, fun), by=x ]
  setkey( dumb, x )
  
  return(dumb)
  
}


