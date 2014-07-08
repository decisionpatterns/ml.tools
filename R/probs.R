#' probs
#' 
#' Get matrix of predicted class probabilities for a model optionally 
#' correcting for imbalances in classes in the training sample. 
#' 
#' @param object a model;
#' @param newdata data; data for which to calculate probaiities
#' @param correct logical; whether to correct the probabilities according to their
#'  frequency of occurance. 
#' 
#' \code{probs} is an S3 generic method.  Functions can be written for 
#' classification models.
#' 
#' Parameter \code{correct} determines whether the final probabilities are 
#' adjusted based on the frequency of occurence in the training data. This is 
#' useful when the training set does not represent the proportion of 
#' observations, but rather is used to define the boundaries of that space. This 
#' is important when using the Brieman column scramble technique.
#' 
#' @return 
#'   numeric matrix of prediction probabilities with the same number of 
#'   rows as \code{newdata} and as many columns as classes in the fitted model
#'   \code{object} 
#' 
#' @seealso
#'   \code{\link[randomForest]{predict.randomForest}}
#'      
#' @examples 
#'   f <- randomForest( Species ~ ., iris[1:125,] )
#'   probs( f, iris )
#'   probs( f, iris, correc=TRUE )
#'   
#' @note 
#'  Although not implemented here, the use \code{probs} could be used to make a 
#'  Bayesian type inference if the frequency of occurence of the final outcome
#'  is known.  
#'       
#'  A better method would probably be to adjust weights during model training
#'       
#' @export  

  probs <- function( object, newdata, ... ) UseMethod( 'probs' )  
  


#' @rdname probs
#' @export
  probs.randomForest <- function( 
    object, newdata, correct=FALSE, ... 
  ) {
    
    prob <- predict( object, newdata, type='prob', ... )
    
    if (correct) {
      # Create class adjustment vector
      tab <- table( object$y )[ colnames(prob) ]
      adjust <- as.numeric(tab)
      names(adjust) <- names(tab)
      
      # corrected <- sweep( prob, 2, adjust, "/")
      # corrected <- sweep( corrected, 2, rowSums(corrected) )
     prob <- sweep( prob, 2, adjust, "/" )
     prob <- sweep( prob, 1, rowSums(prob), "/" )
      
    }
      
    return(prob)  
    
}
