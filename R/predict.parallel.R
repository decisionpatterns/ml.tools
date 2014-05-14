#' predict.parallel - parallel bulk scoring 
#' 
#' Drop in replacement for \cpde{predict} that scores predictive models in parallel. 
#' 
#' @param object a model object for which prediction is desired
#' 
#' @param newdata data frame or matrix containing new data
#' @param ... Additional arguments passed to the predict function
#' 
#' Scores a model on a distrubted cluster using \code{foreach} and 
#' \code{itertools::isplitRows} which chunks the data into one chunck for each 
#' node.   
#' 
#' @return vector of associated predictions
#' 
#' @references 
#'   http://stackoverflow.com/questions/14756662/parallel-prediction-with-cforest-randomforest-prediction-with-dosnow
#'
#' @seealso 
#'   \code{\link[foreach]{foreach}}, \code{\link[itertools]{isplitRows}}
#'   
#' @examples
#'   require(randomForest) 
#'   data(iris)
#'   fit  <- randomForest(Species ~ ., iris)
#'   predict( fit, iris )
#'   predict.parallel( fit, iris )
#'   
#' @export

predict.parallel <- function( object, newdata, ... ) { 

  scores <- 
    foreach( 
       data=isplitRows( newdata, chunks=getDoParWorkers() ) 
      , .inorder=TRUE 
      , .combine=c, .multicombine=TRUE
      # , .packages=c('randomForest','data.table')
      , .export='object'
    ) %dopar% 
      {  
        predict( object, data, ... ) 
      }
  
  return(scores)
}  
    
