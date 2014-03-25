#' Create a prettier variable importance plot 
#;
#' Graph a variable performance plot for a random forest model using ggplot2.
#'  
#' @param model randomForest. Model for which to graph the plot
#' @param nvars integer. Number of variables to plot at most.  (Default: 20)
#' @param title character. Title for the plot.
#' 
#' @export
varImpPlot2 <- function(model, nvars=20, title="Variable Importance") {
  
  # VARIABLE IMPORTANCEs
  imp <- importance(model) #model$classes[12] )
  imp <- data.frame( feature=rownames(imp), importance=imp[,1], row.names=NULL )
  imp <- imp[ order(imp$importance, decreasing=TRUE), ]
  
  # FEATURE
  # imp$feature <- fix.feat.name( imp$feature )
  imp$feature <- factor( imp$feature, levels=rev(imp$feature) )
  
  # IMPORTANCE
  imp$importance <- imp$importance / max(imp$importance )
  
  # SUBSET
  imp <- imp[ 1:min(nvars,nrow(imp)) , ]
  
  ggplot( data=imp, aes(x=importance, y=factor(feature) ) ) +
    geom_point( size=2, colour="darkblue") +
    geom_segment( aes(xend=0, yend=length(importance):1 ), colour="darkgrey")+
    geom_point( size=2, colour="darkblue") +
    xlab( "Relative Importance" ) +
    ylab( "")
  # + opts( title=title, size=1 )
  
}
