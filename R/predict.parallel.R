#' predict.parallel - parallel bulk scoring 
#' 
#' Scores predictive models in parallel.  
#' 
#' @param cl A cluster object from \code{makeCluster}
#' @param object a model object for which prediction is desired
#' @param newdata data frame or matrix containing new data
#' @param ... Additional arguments affecting the prediction
#' 
#' Scores a model on a distrubted cluster. The scores are distributed using 
#' \code{itertools::isplitRows}.  The number of chunks is number of clusters.
#'  
#' 
#' @return a data.table object with key and associated predictions
#' 
#' @references 
#'   http://stackoverflow.com/questions/14756662/parallel-prediction-with-cforest-randomforest-prediction-with-dosnow
#'
#' 
#' @examples
#' 
#'   \dontrun{ fits <- predict( cl, fit, YX ) }
#'   
#' @export

predict.parallel <- function( object, newdata, ... ) { 

  # require(itertools)
  
  chunks <- getDoParWorkers()
  chunksize <- ceiling( nrow(newdata)/(chunks-1) ) 
  
  scores <- foreach( 
       sub_newdata=isplitRows(newdata, chunks=getDoParWorkers() ) 
    , .inorder=TRUE, .combine=c, .multicombine=TRUE
    , .packages=c('randomForest','data.table')
    , .final = function(x) c( predict( fit, newdata[1,] )[0]
    ) %dopar% 
      { 
        predict( object, sub_newdata )
        # sub_newdata[ , list( parent_id, fit=predict(object, newdata=sub_newdata ) ) ] 
      }
  
}  
    