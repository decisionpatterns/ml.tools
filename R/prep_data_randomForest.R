#' Prepare Data for randomForest  
#'
#' @param data data.frame
#' @param max.levels integer; maximum number of levels for categorical variables
#' @param preserve (character) columns not to be prepared
#' @param ... arguments passed to other functions
#'
#' @details 
#' 
#' The Random Forest package requires: 
#'   factors and not strings, 
#'   factors <= 50 levels, 
#'   no missing values,
#'   no +/- Inf values
#'    
#' Additionally, it requires that subsequent factors not have newlevels in the 
#' scoring data.  Each level does not have to be represented, but there needs
#' all levels must be represent in the \code{levels} attribute.
#'    
#' also saves 'values', 
#'   
#' @examples
#'   prep_data_randomForest( iris )
#'   
#' @export

prep_data_randomForest <- function(data, ..., max.levels=50, preserve=character() ) { 
  
  if( length(preserve) > 0 ) pre <- data[ , preserve, with=FALSE ]
  
  data <- coerce_each( data, "character", "factor" )
  data <- coerce_each( data, "ordered", "factor" )    # RF cannot handle ordered.
  data <- coerce_each( data, "difftime", "numeric" ) 
  
  data <- cardinality::reduce_cardinality( data, nlevels=max.levels, ..., keep="__OTHER__" )

  data <- impute(data, fun=median, ... )
  
  if( length(preserve) > 0 ) 
    for( nm in preserve ) 
      data[[ nm ]] <- pre[[ nm ]] 
  
  return(data)
  
}  
