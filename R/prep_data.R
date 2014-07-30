#' Prepare data for use by a particular function
#' 
#' Prepares a data set for use by a particular function
#' 
#' 
#' @param data data.frame.
#' @param fun name of function to prep data for
#' @param ... arguments passed to other functions
#' @return a data frame.
#' @examples
#'   prep_data( iris, randomForest )
#' @export

prep_data <- function(data, fun, ... ) { 

  fun <- as.character( substitute(fun) )  
  fun <- paste0( "dp.misc:::prep_data_", fun, "(data)" )
  
  eval( parse(text=fun) )
  
}   
  
