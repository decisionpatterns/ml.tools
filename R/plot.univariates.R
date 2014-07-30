#' plot.univariates
#' 
#' Create a ggplot of one or more distributions as a comparison
#'
#' @param ... one or more vectors
#' 
#' \code{plot.univariates} is useful for comparing two or more distributions
#' @return 
#'   a ggplot object having one facet and color for each distribution
#' 
#' @examples 
#'  plot.univariates( norm=rnorm(1000, 5), lnorm=rlnorm(10000, 10) ) + scale_x_sqrt()
#' @export 

plot.univariates <- function( ... ) { 
   
  ll <- list( ... )
  
  DF <- null
  for( nm in names(ll) ) { 
    df <- data.frame( dist=nm, value=ll[[nm]] )
    DF <- if( is.null(DF) ) df else rbind( DF, df )
  }

  ggplot( data=DF, aes( x=value, fill=dist ) ) + geom_histogram() + facet_grid( dist ~ . )
  
}
