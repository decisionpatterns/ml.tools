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
#' @seealso reference any functions that are input links to other functions using latex \link{name function}
#' @examples 
#'    calibrate 
#' 
#' @rdname calibrate
#' @export
#' @aliases calibrate
#' 
calibrate <- function(x, y){

  #Bin Methodolgy Sturges Formula  
  if(length(x) > length(y)){
    n <- length(y)
    x <- sample(x, n, replace = TRUE)
  } else {
    n <- length(x)
    y <- sample(y,n, replace = TRUE)
  }
  
  nbins = round(log2(n) + 1)
  
  #Apply binning formula
  x <- cut(x, nbins)
  y <- cut(y, nbins)
    
  #Model data  
  data <- data.table(x=x[order(x)], y=y[order(y)])
  
  model <- lm(x ~ y, data)
 
  #Calibrate Modelled Data
  return (calibrated_y <- predict(model, data))
  
}

