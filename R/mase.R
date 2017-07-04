#' Mean Absolute Scaled Error
#' 
#' @param f forecast;
#' @param x numeric or time-series of observed response
#' @param naive function; forecast method used
#' @param ... additional arguments passed to naive
#' 
#' @details 
#' 
#' The mean absolute scaled error calculates the error relative to a naive
#' prediction 
#' 
#' @references 
#'   \url{https://www.otexts.org/fpp/2/5}
#' 

mase <- function(f, x, naive=naive, ... ) { 
  
  nx <- getResponse(f)
  
  fit.naive <- naive(nx, ...)
  
  if( ! is.forecast(fit.naive) ) { 
    fc.naive <- forecast(fit.naive)  
  } else { 
    fc.naive <- fit.naive    
  }
  
  err <- x - f$mean   
  
  naive.err <- x - fc.naive$mean
  
  mean(abs(err/naive.err))
  
}
