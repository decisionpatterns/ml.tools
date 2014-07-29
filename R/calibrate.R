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
#'    calibrate( 1:10, 1:10 ) # 1:10
#'    calibrate( 1:10, 1:20 )
#'    calibrate( 1:20, 1:10 )
#'    library(testthat)
#'    expect_is( x, 'numeric' ) 
#'    
#' @rdname calibrate
#' @aliases calibrate
#' @export

calibrate <- function(x, y){

  #Bin Methodolgy Sturges Formula  
  if(length(x) > length(y)){
    l <- length(y)
    x <- sample(x, l, replace = TRUE)
  } else {
    l <- length(x)
    y <- sample(y,l, replace = TRUE)
  }
  
  nbins = round(log2(length(x)) + 1)
  
  #Apply binning formula
  x <- cut(x, nbins)
  y <- cut(y, nbins)
    
  #Model data  
  data <- data.table(x=x[order(x)], y=y[order(y)])
  
  model <- lm(x ~ y, data)
 
  #Calibrate Modelled Data
  return( predict(model, data) )
  
}

