#' Calibrate 
#' 
#' Calculates one modelled distribution to closely match the original input distribution
#' 
#' @param x numeric; original distribution 
#' @param y numeric; modeled distribution
#' @param f function; function used to calculate number of bins used to separate original distribution
#'
#' Default bin calculation uses Sturges formula - 
#' Uses linear model to determine the calibrated ...
#' 
#' @return numeric vector; y transformed into basis of x
#' @seealso reference any functions that are input links to other functions using latex 
#' @examples 
#'    org_distribution <- rlnorm(1000) 
#'    modelled_distribution <- rnorm(1000,mean=5)
#'    cal_dist <- calibrate(org_distribution,modelled_distribution)
#' @rdname calibrate
#' @export
#' @aliases calibrate
#' 
calibrate <- function(x, y){

  if(length(x) > length(y)){
    l <- length(y)
    x <- sample(x, l, replace = TRUE)
  } else {
    l <- length(x)
    y <- sample(y,l, replace = TRUE)
  }
  
  #x = sort(x)
  #y = sort(y)
  #df <- data.frame(x,y)
  #mod <- lm(y ~ x, df)
  
  calibrated <- approx(x,y, rule=c(2,2), method='constant')
  
  return (calibrated$y)
  
}