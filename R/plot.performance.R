#' plot.performance 
#' 
#' A drop-in replacement for ROCR::plot.perforamnce that uses ggplot.
#' 
#' @param x Object to plot
#' @param y Missing 
#' 
#' @author Decision Patterns
#' 
#' @return a ggplot2 object
#' 
#' @seealso 
#'   \code{\link[ROCR]{plot.performance}}
#'   \code{\link{as.data.frame.performance}}
#' 
#' @examples 
#'   library(ROCR)
#'   data(ROCR.simple)
#'   pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
#'   perf <- performance(pred,"tpr","fpr")
#'   plot(perf)
#'
#' @import ROCR ggplot2 stringr plotROC
#' @importMethodsFrom ROCR plot
#' @export

setMethod( 'plot', c(x='performance', y='missing' ),
  function( x, y ) { 
    
    df <- as.data.frame.performance(x)
    
    names(df) %<>% str_replace("True positive rate", "TPR")
    names(df) %<>% str_replace("False positive rate", "FPR")
    names(df) %<>% str_replace("Error Rate", "ER")
    
    nms <- names(df)
    
    gg <- ggplot( df, aes_string(nms[[1]],nms[[2]]) ) + 
      coord_equal() + 
      geom_line() + 
      geom_abline( slope = 1, color="red", linetype="dashed" ) 
  
    gg + plotROC::style_roc()
    
  }
)

# .plot.performance <- function( x, y ) { 
#   
#   df <- as.data.frame.performance(x)
#   names(df) %<>% str_replace("True positive rate", "TPR")
#   names(df) %<>% str_replace("False positive rate", "FPR")
#   names(df) %<>% str_replace("Error Rate", "ER")
#   nms <- names(df)
#   
#   gg <- ggplot( df, aes_string(nms[[1]],nms[[2]]) ) + 
#     coord_equal() + 
#     geom_line() + 
#     geom_abline( slope = 1, color="red" ) 
# 
#   gg 
#   
# }


#' Convert ROCR::performance object into a data.frame
#'   
#' @param x ROCR::performance object
#' @param row.names NULL or character; see \code{\link[base]{as.data.frame}}
#' @param optional logical; see \code{\link[base]{as.data.frame}}
#' @param ... additional arguments passed to/from other methods
#'
#' @details 
#' 
#' Converts a performance object into a data.frame of performance measures useful 
#' for creating plots     
#' 
#' @import ROCR
#' @export 
    
as.data.frame.performance <- function(x, row.names=NULL, optional=FALSE, ...) { 

  df <- data.frame(  x@x.values[[1]], x@y.values[[1]], row.names=row.names, check.names=!optional, ...)
  names(df) <- c(x@x.name, x@y.name) 
    
  df
}  