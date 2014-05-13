#' Train Random Forest Model using Multicore
#'
#' Trains a Random Forest Model using Multicore and Foreach
#' 
#' Trains a Random Forest model using multicores.
#'
#' @param ... All parameters passed to \code{randomForest}
#' @param ntree Number of trees to build
#' @param combine (logical). Whether to combine the results using
#' randomForest::combine
#' @param ncores Number of cores to use
#' @return A random forest object.
#' @seealso caret, foreach 
#' @examples
#'   fit <- mcrf( Species ~ ., data=iris, ntree=13 )
#' @keywords model
#' @export

mcrf <- function( ..., ntree=500, combine=TRUE, ncores=parallel::detectCores()-1 ) {

  registerDoParallel(cores=ncores)
  args = list(...) 
  

  # CALCULATE THE NUMBER OF TREES ON EACH CORE
  reps <- rep( floor( ntree/ncores ), ncores )
  mod  <- ntree %% ncores
  if( mod > 0 ) reps[1:mod] <- reps[1:mod] + 1
  
  
  fit <- 
    foreach( ntree=reps, .packages="randomForest" ) %dopar% 
        randomForest( ntree=ntree, ... )
  
  fit <- fit[ class(fit) == "randomForest" ] 

  if( combine ) fit <- Reduce( randomForest::combine, fit ) 

  return(fit)
}

