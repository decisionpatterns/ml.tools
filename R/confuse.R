# ---------------------------------------------------------------------
# CONFUSION MATRIX:
#  MATRIX OF ACTUAL vs. PREDICTED  
#
#     type I (FALSE NEGATIVE )  |  correctly unmatched 
#
#  TODO:
#  
# ---------------------------------------------------------------------

#' 2x2 confusion matrix for logical vectors
#'
#' @param actual logical vector of actual/observed values 
#' @param predicted logical vector of predicted values
#' @param ... additional arguments
#' 
#' By convention, the \code{actual} values are placed in rows and the 
#' predicted values are placed in columns. The \code{dimnames} of the 
#' resulting matrix are consequently \code{actual} and \code{predicted} 
#' 
#' @note 
#' - By convention the actual values are placed in rows, predicted 
#'   values are placed in columns
#' - actual 
#' 
#' @return 2x2 confusion matrix / contingency table
#'   
#' @seealso \code{table} for cross-classifying factors
#' @rdname confuse
#' @export
  confuse <- function( actual, predicted ) UseMethod( 'confuse' )


#' @rdname confuse
#' @export
  confuse.default <- function( actual, predicted, na.rm=TRUE ) {
  
    matrix( 
      c(
        sum( actual & predicted, na.rm=na.rm ), 
        sum( ! actual & predicted, na.rm=na.rm ), 
        sum( actual  & ! predicted, na.rm=na.rm ), 
        sum( ! actual & ! predicted, na.rm=na.rm  ) 
      ), 
      nrow=2  , 
      dimnames = list( actual=c('TRUE', 'FALSE'), predicted=c('TRUE', 'FALSE') )
    )
  
  }


#' @rdname confuse
#' @export
  confuse.categories <- function(actual,predicted) {
    
    actual <- as.character(actual)
    predicted <- as.character(predicted)
    
    if( length(actual) != length(predicted) )
      stop("actual and predicted vectors are of unequal length.")
    
    uniq <- unique( c(actual, predicted) )
    
    
    m <- matrix( 
      0, nrow=length(uniq), ncol=length(uniq) , 
      dimnames=list(Actual=uniq, Predicted=uniq) 
    )
    
    for( a in uniq ) 
      for( p in uniq )
        m[a,p] <- sum( actual == a & predicted == p )  
    
    return(m)
  }


# confuse.logical <- function (actual, predicted) 
# {
#
#  matrix( 
#    c(
#      sum(actual & predicted), sum(!actual & predicted ), 
#      sum(actual & ! predicted), sum(!actual & !predicted)
#    ), 
#    nrow = 2, dimnames = list(actual = c("MATCH", "NON-MATCH"), 
#    predicted = c("MATCH", "NON-MATCH"))
#  )
# }

#' @rdname confuse
#' @export
  confuse.character <- function(...) confuse.catergories(...)

#' @rdname confuse
#' @export
  confuse.factor    <- function(...) confuse.catergories(...)



