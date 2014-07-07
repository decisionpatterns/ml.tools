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
  

#' randomForest requires 
#' * factors and not strings.
#' * factors <= 32 levels    
#' * no missing values
#' * also saves 'values' 
#' * Inf, -Inf not allowed (at least not in caret )
#' * predict additionally requires that there are no new levels in 
#'   \code{newdata}. Each level does not have to be represented, but there needs
#'   all levels must be represent in the \code{levels} attribute.
#' @param data data.frame.
#' @param preserve (character) columns not to prepare.
#' @param ... arguments passed to other functions
#' @max
#' @examples
#'   # -tk
#' @export

prep_data_randomForest <- function(data, ..., max.levels=32, preserve=character() ) { 
   
  if( length(preserve) > 0 ) pre <- data[ , preserve, with=FALSE ]
  
  data <- coerce_each( data, "character", "factor" )
  data <- coerce_each( data, "ordered", "factor" )    # RF cannot handle ordered.
  data <- coerce_each( data, "difftime", "numeric" ) 
  # data <- add_level( data, "__OTHER__")  unnecessary with reduce_cardinality( keep = ...)
  data <- reduce_cardinality( data, nlevels=max.levels, ..., keep="__OTHER__" )
  # attr( data, "prototype" ) <- make_prototype(data)
  # proto <- DataPrototype(data)
  
  data <- impute(data, fun=median, ... )
  
  if( length(preserve) > 0 ) for( nm in preserve ) 
    data[[ nm ]] <- pre[[ nm ]] 
  
  return(data)
  
}  

# iris <- iris; iris[ 1:10, "Petal.Length" ] <- NA



# randomForest <- function(data, ...) {
#   
#   
#   # data <- coerce_each(data, "character", "factor")
#   data <- reduce_cardinality( data, nlevels=32, ... )
#   data <- impute(data, fun=median, ... )
#   
#   data <- 
#   
#   return(data)
#   
# }
