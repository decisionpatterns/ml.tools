setOldClass( 'train' )   # set class for caret

#' Assess the fit between a value and its estimate
#' 
#' Assesses the fit between a value and its estimate using a variety of measures
#' 
#' @param x vector of outcomes
#' @param y vector of predictions
#' @param ... additional arguments 
#' @param na.action function to be used when arguments are absent
#' @param format (See scales package.)
#' 
#' Calculates a variety of perfomance measures for a model fit.
#' @export
#' @rdname assess-methods 

setGeneric( "assess", function(x, y, ... ) standardGeneric('assess') )


# Methods:
# logical-logical, factor-factor uses confusion matrix
# model, ANY, ...
# ANY, model, ...


#' @rdname assess-methods
#' @aliases assess,integer,integer-method

setMethod( 
  'assess', c('integer', 'integer' ),
  function( x, y, na.action=na.exclude, ... ) 
    assess( as.numeric(x), as.numeric(y), na.action, ... )   
)


#' @rdname assess-methods
#' @aliases assess,numeric,numeric-method
 
setMethod( 
  'assess', c('numeric', 'numeric' ),
  function( x, y, na.action=na.exclude, format=scales::comma, ... ) {
    
    data <- cbind( x, y ) 
    # data <- y 
    # data <- data.frame( x, y )
  
    data <- na.action( data, ... )
    x = data[,1]
    y = data[,2]
    
    err = x - y
    abs_err = abs( err )
    pct_err = abs_err / x   
    
    cat( sep="\n"
         # , nm
         , paste0( "r-squared: ", round( R2( x, y ), 3 ) )
         , paste0( "mae      : ", format( mae( x, y ) ) )  
         , paste0( "rmse     : ", format( rmse( x, y ), nsmall=0 ) )  
         , paste0( "median-ae: ", format( median(abs_err), nsmall=0 ) )    
         , paste0( "mean PE  : ", percent( mean(pct_err) ) )
         , paste0( "median PE: ", percent( median(pct_err) )  )
    )  
  }
)


#' @details \code{train}, \code{data.frame} method evaluates the model 
#' @rdname assess-methods
#' @aliases assess,train,data.frame-method 
setMethod( 
  'assess' , c('train','data.frame'),
  function(x,y, na.action=na.exclude, ... ) {
  
     form    <- formula(x) 
     actual  <- eval( lhs(form), y )

     estimated <- predict( x, y )

     return( assess( actual, estimated ) ) 
 
  } 
)
