#' Cut vector into bin
#' 
#' Cut vector into binned factor
#' 
#' @param x numeric; vector to be divided into bins
#' @param breaks either 
#'   - single integer for the number of breaks 
#'   - function that returns a single integer 
#'   
#' @param ... arguments passed onto cut
#'  
#' @return 
#' factor with \code{nbin} levels arranged Values correspond to the bin the o 
#' 
#' @examples 
#' 
#' rnorm(50) %>% cut_n(dig.lab=1)
#' rnorm(50) %>% cut_n( nbin=nclass.FD )
#' 
#' @export

cut_n <- function(x, nbin=nclass.scott, relax=c(-Inf,Inf), ...) {
  
  if( is.function(nbin) )
    nbin <- nbin(x)
      
  cut( x, breaks=breakpoints(x, nbin=nbin, relax=relax), ... )
}

#' Special standard evaluation of cut_n
#' 
#'
#' 
#' @param .data environment to evaluate \code{x} in. 
#' This is needed for use in \code{j} and \code{by} in data.table when this 
#' should be explicitly passed \code{.SD}
#' 
#' @examples 
#' 
#' mpg[ , .(x=col %>% cut_n_(5,.SD)) ]     # works
#' 
#' @rdname cut_n
#' @export 

cut_n_ <- function(
  x, nbins, .data=parent.frame()  
) { 
  cut( .data[[x]], breaks=breakpoints( .data[[x]], nbins=nbins )  )  
}


#' @seealso 
#'   \code{\link[base]{nclass.FD}}, \code{\link[base]{nclass.scott}}, 
#'   \code{\link[base]{nclass.Sturges}} for data-driven methods for determining
#'   the number of breaks.   
#'   
#' @examples 
#' 
#' breakpoints( 1:10, nbins=5)
#' 
#' @export 

breakpoints <- function(x, nbins, relax=c(-Inf,Inf) ) { 
 
    min_x <- min( x, na.rm=TRUE )
    max_x <- max( x, na.rm=TRUE )
    
    ret <- seq( min_x, max_x, (max_x-min_x)/nbins )
    
    if( ! is.null(relax) )
      ret <- relax_breaks(ret,relax)
  
    return(ret)
}


#' Relax breakpoints
#' 
#' Adjust the beginning and relaxing values in a vector to \code{relax}
#' 
#' @param x numeric; strictly monotonic vector of breakpoints
#' @param relax numeric vector of length 2 containing the minimum 
#' 
#' @details 
#'
#' This is mostly used for constructiong breakpoints; there may be other uses. 
#' Relaxing break points is important as it allows for new data outside the 
#' range to be incorpotated into a model
#' 
#' @export

relax_breaks <- function(x, relax = c(-Inf,+Inf) ) {
  
  if( ! all( diff(x) > 0 ) ) warning( "x is not strictly monotonic." ) 
  
  if( min(x) < relax[[1]] )  warning( "minimum of x is less than the min relax")
  if( max(x) > relax[[2]] )  warning( "maximum of x is less than the max relax")
      
    
  x[[1]] <- relax[[1]]
  x[[length(x)]] <- relax[[2]] 
  x
  
}
