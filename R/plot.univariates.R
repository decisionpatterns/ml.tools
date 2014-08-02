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
#'  plot.univariates( norm=rnorm(1000, 5), lnorm=rlnorm(10000, 10) ) + 
#'    scale_x_sqrt()
#'    
#'  plot.univariates( rnorm(1000, 5), rlnorm(10000, 10) ) + scale_x_sqrt()
#'  
#' @export 

plot.univariates <- function( ... ) { 
   
  ll <- list( ... )
  
  # TRAP FOR MISSING NAMES
  nms <- names(ll)
  if( is.null( nms ) ) names(ll) <- paste0( 'x', 1:length(ll) )
  if( any( is.na(nms) ) )
    names( ll )[ which( is.na(nms) ) ]  <- paste0( 'x', 1:sum(is.na(nms) ) )    
  
  DF <- NULL
  for( nm in names(ll) ) { 
    df <- data.frame( dist=nm, value=ll[[nm]] )
    DF <- if( is.null(DF) ) df else rbind( DF, df )
  }

  ggplot( data=DF, aes( x=value, fill=dist ) ) + geom_histogram() + facet_grid( dist ~ . )
  
}
