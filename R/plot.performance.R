#' plot.performance 
#' 
#' A drop-in replacement for ROCR::plot.perforamnce that uses ggplot.
#' 
#' @author Decision Patterns
#' 
#' @examples 
#' 
#' qplot( perf@x.values[[1]], perf@y.values[[1]])
#' 
#' @import ggplot2 
#' @return a ggplot2 object
#' @seealso 
#'   \code{\link[ROCR]{plot.performacne}}
#' 
#' 
#' @examples 
#'   library(ROCR)
#'   data(ROCR.simple)
#'   pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
#'   perf <- performance(pred,"tpr","fpr")
#'   plot.performance(perf)
#' 
#' @import ROCR stringr.tools
#' @exportMethod plot performance  

plot.performance <- function( x, y ) { 
  
  df <- as.data.frame.performance(x)
  names(df) %<>% str_replace("True positive rate", "TPR")
  names(df) %<>% str_replace("False positive rate", "FPR")
  names(df) %<>% str_replace("Error Rate", "ER")
  nms <- names(df)
  
  gg <- ggplot( df, aes_string(nms[[1]],nms[[2]]) ) + 
    coord_equal() + 
    geom_line() + 
    geom_abline( slope = 1, color="red" ) 

  gg  
}

  
    
as.data.frame.performance <- function(x) { 

  df <- data.frame(  x@x.values[[1]], x@y.values[[1]] )
  names(df) <- c(x@x.name, x@y.name) 
    
  df
}  