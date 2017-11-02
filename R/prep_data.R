#' Prepare data for use by a particular function
#' 
#' Prepares a data set for use by a particular function
#' 
#' @param data data.frame.
#' @param fun name of function to prep data for
#' @param ... arguments passed to other functions
#' @return a data frame.
#' @examples
#'   # -tk
#' 
#' @export

prep_data <- function(data, fun, ... ) { 

  fun <- as.character( substitute(fun) )  
  fun <- paste0( "ml.tools:::prep_data_", fun, "(data)" )
  
  eval( parse(text=fun) )
  
}   
