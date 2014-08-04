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
#'  plot.univariates( 1:100, rlnorm=rlnorm(10000, 10) ) + scale_x_sqrt()
#'  plot.univariates( 1:100, rnorm(100))
#' @export 

plot.univariates <- function( ... ) { 
   
  ll <- list( ... )
  
  # TRAP FOR MISSING NAMES
  nms_1 <- names(ll)
  nms_2 <- as.character( dots(...) ) 
  
  if( is.null(nms_1)) nms_1 <- nms_2 
  nms_1[ is.na(nms_1) | nms_1 == '' ] <- 
    nms_2[ is.na(nms_1) | nms_1 == '' ]
    
  names(ll) <- nms_1  
  
  DF <- NULL
  for( nm in names(ll) ) { 
    df <- data.frame( dist=nm, value=ll[[nm]] )
    DF <- if( is.null(DF) ) df else rbind( DF, df )
  }

  ggplot( data=DF, aes( x=value, fill=dist ) ) + geom_histogram() + facet_grid( dist ~ . )
  
}
