#' parRandomForest
#'
#' provides parRandomForest and a parallel drop-in replacement for 
#' \code{randomForest}using foreach/doParallel
#' 
#' @param ... parameters passed to \code{randomForest}
#' @param ntree integer; number of trees train
#' 
#' Trains a Random Forest model using a \code{foreach} cluster. The cluster must
#' first be registered with \code{registerDoParallel} 
#'
#' @return A \code{randomForest} object.
#' 
#' @references 
#'   http://stackoverflow.com/questions/5571774/what-is-the-easiest-way-to-parallelize-a-vectorized-function-in-r/15414748#15414748 
#'
#' @seealso 
#'   \code{\link[randomForest]{randomForest}}, 
#'   code{\link[caret]{train}}, 
#'   \code{\link[doParallel]{registerDoParallel}},
#'   \code{\link[foreach]{foreach}}
#'     
#' @examples
#'   registerDoParallel(4)
#'   
#'   #EXPLICIT:
#'   fit <- parRandomForest( Species ~ . , iris, ntree=200)
#'   
#'   # DROP-IN REPLACEMENT
#'   fit <- randomForest( Species ~ . , iris, ntree=200)
#'   
#'   # CARET 
#'   fit <- train( Species ~ . , iris, ntree=200 )
#'   
#' @import randomForest foreach doParallel
#' @rdname parRandomForest
#' @export

parRandomForest <- function( ..., ntree=500 ) { 

  # CALCULATE THE NUMBER OF TREES ON EACH WORKER
    nworkers <- getDoParWorkers()
    reps <- rep( floor( ntree/nworkers ), nworkers )
    mod  <- ntree %% nworkers 
    if( mod > 0 ) reps[1:mod] <- reps[1:mod] + 1
    reps <- reps[ reps > 0 ]  
  
  fit <- 
    foreach( 
        ntree=reps
        , .combine=randomForest::combine
        , .multicombine=TRUE 
    ) %dopar% 
      randomForest::randomForest( ..., ntree=ntree )

  return(fit)
  
}




#' @rdname parRandomForest
#' @export
randomForest <- function( ... ) { 
  # message( "...using ml.tools::randomForest" ) 
  parRandomForest(...)
}


