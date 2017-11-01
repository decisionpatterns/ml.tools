#' confuse
#' 
#' Create a confusion matrix for categorical values
#'
#' @param obs vector of observed/actual values 
#' @param pred vector of predicted values
#' @param na.rm whether to removing missing values
#' 
#' @param ... additional arguments
#' 
#' By convention, the \code{observed} values are placed in rows and the 
#' predicted values are placed in columns. The \code{dimnames} of the 
#' resulting matrix are consequently \code{observed} and \code{predicted} 
#' 
#' @usage 
#' 
#' Method exists for caret::train() objects 
#' 
#' 
#' @note 
#' - By convention the observed values are placed in rows, predicted 
#'   values are placed in columns
#' - observed 
#' 
#' @return 2x2 confusion matrix / contingency table
#'   
#' @seealso \code{table} for cross-classifying factors
#' @rdname confuse
#' @export

  confuse <- function( obs, pred ) UseMethod( 'confuse' )


#' @rdname confuse
#' @export
  confuse.default <- function( obs, pred, na.rm=TRUE ) {
  
    matrix( 
      c(
        sum( obs & pred, na.rm=na.rm ), 
        sum( ! obs & pred, na.rm=na.rm ), 
        sum( obs  & ! pred, na.rm=na.rm ), 
        sum( ! obs & ! pred, na.rm=na.rm  ) 
      ), 
      nrow=2  , 
      dimnames = list( observed=c('TRUE', 'FALSE'), predicted=c('TRUE', 'FALSE') )
    )
  
  }


#' @examples 
#'   confuse.categories( obs=qw(A,A,B,B), pred=qw(A,B,C,C) )
#'   
#' @rdname confuse
#' @export
  confuse.categories <- function(obs,pred) {
    
    obs <- as.character(obs)
    pred <- as.character(pred)
    
    if( length(obs) != length(pred) )
      stop("obs and pred vectors are of unequal length.")
    
    uniq <- unique( c(obs,pred) )
    
    
    m <- matrix( 
      0, nrow=length(uniq), ncol=length(uniq) , 
      dimnames=list(observed=uniq, predicted=uniq) 
    )
    
    for( a in uniq ) 
      for( p in uniq )
        m[a,p] <- sum( obs == a & pred == p )  
    
    return(m)
  }


# confuse.logical <- function (obs, pred) 
# {
#
#  matrix( 
#    c(
#      sum(obs & pred), sum(!obs & pred ), 
#      sum(obs & ! pred), sum(!obs & !pred)
#    ), 
#    nrow = 2, dimnames = list(observed = c("MATCH", "NON-MATCH"), 
#    predicted = c("MATCH", "NON-MATCH"))
#  )
# }

#' @rdname confuse
#' @export
  confuse.character <- function(...) confuse.catergories(...)

#' @rdname confuse
#' @export
  confuse.factor    <- function(...) confuse.catergories(...)

  
#' @examples 
#'   
#' @rdname confuse
#' @import caret 
#' @export

  confuse.train <- function( obs, pred=NULL ) { 

    # If pred is NULL, look to see if the train object used `savePredictions` 
    if( is.null(pred) ) { 
      fit <- obs 
    
      if( exists("pred", fit ) ) {
        obs <- fit$pred$obs
        pred <- fit$pred$pred 
      
        
      } else { 
        stop()
      }
      
      confuse.categories( obs, pred )  
    
    }
  }
  