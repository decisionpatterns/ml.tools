# ----------------------------------------------------------------------
# FUNCTION: scramble.col
#
#   Column scramble. Determines what variables are important for the 
#   determination of the data.
#
#   Can we create a matrix of the same dimension as the data frame and
#   sort the.
#   We can
# ----------------------------------------------------------------------

#' Column scramble 
#'
#' Scrambles the columns of a matrix or data.frame
#' 
#' @param x Object to scramble the columns of
#' 
#' Scrambles the columns of a matrix or data.frame. This is useful for
#' creating supervised clustering models where there are no examples of
#' FALSE/not-present examples of the data. This is after Breiman.
#'
#' @return 
#' An object with the same class and size of \code{x} in which each 
#' column has been randomly reordered. 
#'
#' @references Brieman's Random Forest paper.
#' 
#' @author Christopher Brown
#' @note Incorporated with permission from the copyright holder.
#'
#' @examples
#'   scramble(iris)
#'
#' @rdname scramble
#' @export scramble


scramble <- function(x) UseMethod('scramble')

#' @rdname scramble
#' @export
scramble.data.frame <- function(x) {

  x. <- as.data.frame( lapply( x, 
    function(x) x[ sample( 1:length(x), replace=TRUE ) ] 
  ) )

  return( x.)

}


    
#' @rdname scramble
#' @export
scramble.matrix <- function(x) {

  x. <- apply( x, 2, function(x) x[ sample(1:length(x)) ] )  
  x. <- cbind( x., FALSE ) 

  x  <- cbind( x, TRUE ) 
  
  ret <- rbind( x, x. )  
  colnames(ret)[ ncol(x) ] <- ".orig" 

  return(ret)

}


