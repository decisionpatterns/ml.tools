#' Calibrate Function
#' 
#' Calibrates a modelled distribution so that it closely matches the 
#' original distribution that was used as input into the model.
#' 
#' @param x numeric; original distribution in the form of a vector
#' @param y numeric; modeled distribution in the form of a vector
#'
#' The function first sorts the distributions then ensures they're the same size
#' then uses approx to linear interpolate the modelled distribution into the 
#' same form as the original distribution.
#'  
#' @return numeric vector; y transformed into basis of x
#' @seealso sort, seq, approx 
#' @examples 
#'    org_distribution <- rlnorm(1000) 
#'    modelled_distribution <- rnorm(1000,mean=5)
#'    cal_dist <- calibratefun(org_distribution,modelled_distribution)
#'    calibratefun(1:10, 1:10)
#'    calibratefun(1:10, 1:20)
#'    calibratefun(1:20, 1:10)
#' @rdname calibratefun
#' @export
#' @aliases calibratefun
#' 
calibratefun <- function(x, y){

  x = sort(x)
  y = sort(y)
  x = x[seq(1, length(x), length.out = min(length(x),length(y)))]
  y. = y[seq(1, length(y), length.out = min(length(x),length(y)))]

  calibratefun <- approxfun(y.,x,rule=c(2,2), method='constant')
  
  return (calibratefun)
  
}
