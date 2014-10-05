#' nodes 
#' 
#' Report the nodes numbers
#' 
#' @param object 
#' @param newdata data.frame of data 
#' 
#' @return 
#'   numeric matrix containing one row per observation, one column per tree in 
#'   the forest. The values indicate which node of each tree the observation 
#'   falls in.   
#' 
#' @seealso 
#'   \code{\link{predict}}
#'   
#' @examples
#'   fit <- randomForest( Species ~ . , iris )   
#'   nodes( fit, iris ) 
#'   
#' @export

setGeneric( 'nodes', function(object, newdata) standardGeneric('nodes') )

setOldClass('randomForest')
setOldClass( c('randomForest.formula', 'randomForest') )

#' @export 
setMethod( 'nodes', signature( 'randomForest', 'data.frame' ),
  function( object, newdata ) { 
    attr( predict( object, newdata, nodes=TRUE ), 'nodes' )
  }           
)
