#' getImp
#' 
#' Get most important features from an RF model in order of importance
#' 
#' @param model A rf model or at least something that has an \code{importance} 
#' @param n number of features to extract; the default is to extract all features
#' 
#' @return factor
#' 
#' @seealso \code{\link[randomForest]{randomForest}}
#' 
#' @examples
#'  # -tk
#' 
#' @export 

getImp <- function(model, n=length(model$importance) ) {

    imp <- randomForest::importance(model)
    imp <- data.frame(feature = rownames(imp), importance = imp[, 
        1], row.names = NULL)
    imp <- imp[order(imp$importance, decreasing = TRUE), ]
    imp$feature <- factor(imp$feature, levels = rev(imp$feature))
    imp$importance <- imp$importance/max(imp$importance)
    imp <- imp[1:min(n, nrow(imp)), ]

    imp$feature
  
}
