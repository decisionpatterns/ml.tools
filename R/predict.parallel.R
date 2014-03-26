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

predict.parallel <- function( cl, object, newdata, ... ) { 

  require(itertools)
  
  chunks <- length(cl)
  chunksize <- ceiling( nrow(newdata)/(chunks-1) ) 
  
  foreach( 1:chunks, sub_new_data=isplitRows(newdata, chunkSize=chunksize) 
    , .inorder=TRUE, .combine=rbind2, .packages=c('randomForest','data.table') 
    ) %dopar% 
  { 
    sub_new_data[ , list( parent_id, fit=predict(object, newdata=sub_new_data ) ) ] 
  }
  
}  
    