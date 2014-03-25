#' Prepare data for training and scoring of a caret model
#' 
#' Prepares data for scoring of a caret model by making sure scoring data is 
#' similar to trainingData. 
#' 
#' Prepares data for scoring does the following
#'  - checks for missing trainingData
#'  - checks missing columns in newdata (n.b. this would be caught by predict)
#'  - reduce new levels to OTHER value.
#'  - drop inused levels
#'  - Impute missing values
#'  
#' @param data data.table to prepare
#' @param model train caret model to emulate
#' 
#' @return a data.table that can (hopefully) be used by the \code{predict} 
#' method
#' 
#' @seealso prep_data, reduce_cardinality
#' 
#' @note OTHER could be \code{NA}, but there is a difference between missing
#' and present but reduced. This maintains 
#' 
#' @export

prep_score <- function( data, model, OTHER="__OTHER__" ) { 
  
  # proto <- model 
  # nms_data  <- names(data)
  # nms_proto <- names(proto)
  
  # CHECK FOR MISSING trainingData
  #  Get required names
  if( exists( "trainingData", model ) ) 
    nms_req <- get.vars( rhs(formula(model)), model$trainingData ) else
    stop( "No training data available ")
  
  # CHECK FOR MISSING COLUMNS
  if( ! all( nms_req %in% names(data) ) ) { 
    missing <- setdiff( nms_req, nms_data )
    warning( substitute(data), " has missing values : ", paste(missing,collapse=", ") )
  }
  
  
  data <- prep_train(data) 
  
  # NEED TO prep Required(1), Factors(2) 
    wh <- names( which( names(data) %in% nms_req & sapply( data, is.factor ) ) )
  
  # FOR EACH REQUIRED FACTOR
  #  - Change any new levels to OTHER
  #  - Drop unused levels
  
  for( nm in wh ) {
    data[[ nm ]]    
    proto.levels <- levels( model$trainingData[[ nm ]] )
    data[[nm]][ ! data[[nm]] %in% proto.levels ] <- OTHER
    data[[nm]] <- droplevels( data[[nm]] )
  }
    
  # Imputations 
  
  return(data)
  
}


#' randomForest requires 
#' * factors and not strings.
#' * factors <= 32 levels    
#' * no missing y values
#' * also saves 'values' 
#' * Inf, -Inf not allowed (at least not in caret )
#' * predict additionally requires that there are no new levels in 
#'   \code{newdata}. Each level does not have to be represented, but there needs
#'   all levels must be represent in the \code{levels} attribute.
#' @param data data.frame.
#' @param preserve (character) columns not to prepare.
#' @param ... arguments passed to other functions
#' @seealso \code{\link{prep_data}}
#' @examples
#'   # -tk
#' @export
prep_train <- function( data, ..., preserve=character() ) { 
  
  if( length(preserve) > 0 ) pre <- data[ , preserve, with=FALSE ]
  
  
  data <- coerce_each(data, "character", "factor")
 
  # By having keep="__OTHER__", this is automatically added to the allowable 
  # levels of the variables.  
  data <- reduce_cardinality( data, nlevels=32, ..., keep="__OTHER__" )
  
  data <- impute(data, fun=median, ... )
  
  if( length(preserve) > 0 ) for( nm in preserve ) 
    data[[ nm ]] <- pre[[ nm ]] 
  
  return(data)
  
}  

  