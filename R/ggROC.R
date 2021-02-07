#' ggROC
#'
#' Produce a fancy ROC curve using ggplot2.
#'   
#' @param x object
#' @param y depends on typeof(x). See details.
#' @param measure string; measure used with \code{ROCR::performance}
#' @param x.measure string; measure used with \code{ROCR::performance}
#' @param ... additional arguments passed to [ROCR::performance()]
#' 
#' @details 
#' 
#' \code{ggROC} is an S3 generic function for producing ggplot based ROC curves.
#' 
#' @seealso 
#' 
#'  - [ROCR::plot.performance()](ROCR::plot.performance())
#'  - [SO::roc-curve-from-training-data-in-caret](https://stackoverflow.com/questions/31138751/roc-curve-from-training-data-in-caret)
#'  - [plotROC package](https://cran.r-project.org/package=plotROC)
#'  - [ROCR::plot.performance()]
#' 
#' @examples
#' 
#'   library(ROCR)
#'   data(ROCR.simple)
#'   ggROC( ROCR.simple$predictions, ROCR.simple$labels)
#'   
#'   # perf <- performance(pred,"tpr","fpr")
#'   # plot.performance(perf)
#' 
#' @md
#' @import ROCR data.table
#' @export

ggROC <- function(x,y,...) UseMethod('ggROC') 


#' @details 
#' For numeric objects, \code{x} is a predicted values/probabilities, \code{y} 
#' is the labels. 
#' 
#' @examples 
#' 
#'  
#' 
#' @aliases  ggROC
#' @rdname ggROC
#' @export 

ggROC.numeric <- function(x, y, measure="tpr", x.measure="fpr", ... ) { 
  
  pred <- ROCR::prediction(x,y,...)
  perf <- ROCR::performance(pred, measure=measure, x.measure=x.measure, ... )
  plot(perf) + 
    geom_abline( slope=1, intercept=0, linetype="dashed", color="red") 
  
}

#' @details 
#' 
#' This plots all resampled values that match the `bestTune` parameters.
#' 
#' @examples 
#'  data(iris)
#'  iris$setosa <- factor( ifelse(iris$Species == 'setosa', 'setosa', 'other') )
#'  
#'  ctrl <- trainControl(savePrediction=TRUE, classProbs = TRUE) # Important
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
  
  # Get Only bestTune Resamples
  pred <- x$pred 
  pred %>% setDT() 
  
  bestTune <- x$bestTune
  bestTune %>% setDT()
  
  tbl <- pred[ bestTune, on=names(bestTune) ]   # Onl
  
  # tbl <- tidyr::gather_(x$pred, key_col="prob_label", value_col="prob", gather_cols=x$levels )
  # tbl <- tbl %>% subset( pred != prob_label )
  
  # ROCR only handles binary classification models...
  # reduce the classes to correct or not which is a pretty good emulation 
  
  prob <- rep(NA_real_,nrow(tbl))
  for( i in x$levels ) {
    wh <- tbl$pred==i
    prob[wh] <- tbl[ wh, ..i][[i]] 
  }  
    
  if( length(unique(tbl$obs)) > 2 )
    tbl$obs <- tbl$obs == tbl$pred
  
  
  
  ggROC( prob, tbl$obs, ... ) 
  
}


