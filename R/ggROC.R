#' ggROC
#'
#' Produce a ROC curve using ggplot2
#'   
#' @param x object
#' @param y depends on typeof(x). See details.
#' @param measure string; measure used with \code{ROCR::performance}
#' @param x.measure string; measure used with \code{ROCR::performance}
#' @param ... additional arguments 
#' 
#' @details 
#' 
#' \code{ggROC} is an S3 generic function for producing ggplot based ROC curves.
#' 
#' @seealso 
#' 
#'  - [ROCR::plot.performance](ROCR::plot.performance())
#'  - [SO::roc-curve-from-training-data-in-caret](https://stackoverflow.com/questions/31138751/roc-curve-from-training-data-in-caret)
#'  - [plotROC package](https://cran.r-project.org/package=plotROC)
#'  
#' \code{\link{plot.performance}}
#' 
#' @examples
#' 
#'   library(ROCR)
#'   data(ROCR.simple)
#'   ggROC( ROCR.simple$predictions, ROCR.simple$labels)
#'   perf <- performance(pred,"tpr","fpr")
#'   plot.performance(perf)
#' 
#' @md
#' @import ROCR  
#' @export

ggROC <- function(x,y,...) UseMethod('ggROC') 


#' @details 
#' For numeric objects, \code{x} is a predicted values/probabilities, \code{y} is the labels. 
#' 
#' @examples 
#' 
#' 
#' 
#' @aliases  ggROC
#' @rdname ggROC
#' @export 

ggROC.numeric <- function(x, y, measure="tpr", x.measure="fpr", ... ) { 
  
  pred <- prediction(x,y,...)
  perf <- performance(pred, measure=measure, x.measure=x.measure, ... )
  plot(perf)
  
}

#' @examples 
#'  ctrl <- trainControl(savePrediction=TRUE, classProbs = TRUE)
#'  fit <- caret::train( Species ~ ., iris, method="rf", metric="Kappa", trControl=ctrl )
#'  ggROC(fit)
#'  
#' @rdname ggROC
#' @aliases ggROC.caret  
#' @import caret 
#' @import tidyr
#' @export

ggROC.train <- function(x, y, ...) {
 
  if( x$modelType != "Classification" )
    stop( "ggROC curves are only for Classification models" )
    
  if( ! exists('pred', x) || is.null(x$pred) )
    stop( "No predictions saved.")
  
  # Find the columns before "rowIndex"
  # idx_rowIndex <- x$pred  %>% names() %>% equals('rowIndex') %>% which()
  # x$pred %>% names() %>% .[1:row]
  
  gather_cols <- c( )
  tbl <- tidyr::gather_(x$pred, key_col="prob_label", value_col="prob", gather_cols=x$levels )
  tbl <- tbl %>% subset( pred != prob_label )
  
  # ROCR only handles binary classification models...
  # reduce the classes to correct or not which is a pretty good emulation 
 
  if( length(unique(tbl$obs)) > 2 )
    tbl$obs <- tbl$obs == tbl$pred
  
  ggROC( tbl$prob, tbl$obs, ... ) 
  
}
