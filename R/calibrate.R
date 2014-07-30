#' Calibrate 
#' 
#' Calculates one modelled distribution to closely match the original input distribution
#' 
#' @param x numeric; original distribution 
#' @param y numeric; modeled distribution
#'
#' Uses Sturges formula to...
#' Uses linear model to determine the calibrated ...
#' 
#' @return numeric vector; y transformed into basis of x
#' @seealso reference any functions that are input links to other functions using latex 
#' @examples 
#'    calibrate 
#' 
#' @rdname calibrate
#' @export
#' @aliases calibrate
#' 
calibrate <- function(x, y, f = function(l){round(log2(length(l)) + 1)}){

  #Bin Methodolgy Sturges Formula  
  if(length(x) > length(y)){
    l <- length(y)
    x <- sample(x, l, replace = TRUE)
  } else {
    l <- length(x)
    y <- sample(y,l, replace = TRUE)
  }
  
  nbins <- f(x) 
  calibrated_y <- split(y,cut(x, nbins))
  
  return (calibrated_y)
  
}