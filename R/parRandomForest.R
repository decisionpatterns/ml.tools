#' parRandomForest
#'
#' provides parRandomForest and a parallel drop-in replacement for 
#' \code{randomForest}using foreach/doParallel
#' 
#' @param ... parameters passed to \code{randomForest}
#' 
#' Trains a Random Forest model using a \code{foreach} cluster. The cluster must
#' first be registered with \code{registerDoParallel} 
#'
#' If \code{getOption('verbose')} is TRUE, a message is printed. This helps to
#' check usage in package like 'caret'. 
#' 
#' @return A \code{randomForest} object.
#' 
#' @references 
#'   \url{http://stackoverflow.com/questions/5571774/what-is-the-easiest-way-to-parallelize-a-vectorized-function-in-r/15414748#15414748}
#'
#' @seealso 
#'   \code{\link[randomForest]{randomForest}} \cr
#'   \code{\link[caret]{train}} \cr
#'   \code{\link[doParallel]{registerDoParallel}} \cr
#'   \code{\link[foreach]{foreach}} \cr
#'     
#' @examples
#'   registerDoParallel(4)
#'   
#'   # EXPLICIT:
#'   fit <- parRandomForest( Species ~ . , iris, ntree=10)
#'   
#'   # DROP-IN REPLACEMENT
#'   fit <- randomForest( Species ~ . , iris, ntree=20, mtry=4)
#'   
#'   # CARET 
#'   fit <- train( Species ~ . , iris, ntree=200 )
#'   
#' @import randomForest foreach doParallel base.tools
#' @rdname parRandomForest
#' @export

parRandomForest <- function( ... ) { 

  li <-list(...)
  if( ! 'ntree' %in% names(li) ) {
    ntree <- 500
    li$ntree <- 1
  } else { 
    ntree <- li$ntree
    li$ntree <- 1
  }
   
  # rf <- randomForest::randomForest
  # rf <- base.tools::modify_args( rf, li )
  
  if( getOption( 'verbose', FALSE ) ) { 
    message("Using parRandomForest")
    
    # message(
    #      "  rows         : ", nrow( fit$trainingData ), "\n"
    #   ,  "  cols         : ", ncol( fit$trainingData ), "\n"
    #   ,  "  cardinality  : ", sum( cardinality(fit$trainingData) ), "\n"
    # )
  }
  
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
      { 
        li$ntree <- ntree
        base.tools::do( randomForest::randomForest, li )
      }

  return(fit)
  
}


#' @rdname parRandomForest
#' @export
randomForest <- function( ... ) { 
  parRandomForest(...)
}


