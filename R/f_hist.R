#' Create histogram features 
#' 
#' Create histogram features 
#' 
#' @param .data data
#' @param x string or unquoted name of variable used for histogram 
#' @param by character; name of variables for 
#' @param fun function; function that takes a vector and returns a named list. 
#' See Details.  
#' @param ... arguments passed to fun
#' 
#' @details 
#' 
#' Histogram features are features that purport distributional information. 
#' They are similar to dummy variables in the sense that a single variable gets
#' expanded into multiple features, unlike dummy variable however, the values 
#' are not coded as 0|1. They are coded as a summary statistic similar such as 
#' count, density, frequency or break point similar to the way that histograms 
#' work. 
#' 
#' Most often, histogram features are accompanied by a `by` that ...  
#'  
#' \code{x} may be a string or an unquoted value. This value is evaluated on 
#' data and serves as the tartet of 
#'   
#' \code{by} is a character vector used to define the grain of the feature. 
#' The resulting data table's key is set to this value. If not key is provided,
#' no aggregations are p 
#'    
#' The summary function \code{fun} must accept a vector specified by \code{x} 
#' and return a named list. The element of the list become the histogram 
#' features values; the names will become the feature names.
#' 
#' 
#' 
#' @examples 
#' 
#' library(data.table)
#' data(mtcars)
#' setDT(mtcars)
#' 
#' f_hist( mtcars, "mpg", "cyl", bin_count_list )
#' 
#' mtcars %>% f_hist("mpg", by="cyl")
#' mtcars %>% f_hist(mpg, by="cyl") 
#' 
#' mtcars %>% f_hist("mpg", c("cyl","gear"), bin_count_list )
#' 
#' @import base.tools data.table
#' @export 

f_hist <- function( .data, x, by=NULL, fun=bin_count_list , ...) { 

  .data %>% setDT 
  # HANDLE unquoted x ... convert to character
  if( exists('x') && ! base.tools::is.string(x) ) {  
    x <- substitute(x) %>% as.character
  }
  
  ret <- .data[ by=by, , fun(x,envir=.SD, ...) ]
  ret %>% setkeyv(by)  
  return(ret)
  
}


