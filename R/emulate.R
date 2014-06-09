#' Make one object emulate another
#' 
#' Make one object more like another 
#' 
#' @param x object to emulate
#' @param y object to effect and return
#' @param ... additional parameters 
#' 
#' \code{emulate} makes \code{x} more like \code{y} so that \code{x} can be 
#' used in place of \code{y} with changing the data as little as possible.  This
#' generally means coercing to the type of \code{y} and only containing the 
#' values that are in \code{y}.
#' 
#' For the \code{randomForest} method, this requires adding a \code{default}
#' 
#' @return 
#'   Returns \code{x} that emulates \code{y}
#'   
#'   The ANY-ANY method simply returns \code{x}
#'   
#' @export 
#' @docType methods
#' @rdname emulate-methods

setGeneric( 'emulate', function(x, y, ...) standardGeneric('emulate') )

setOldClass( 'randomForest') 

#' @aliases emulate,randomForest,data.table-method
#' @rdname emulate-methods
#' @examples
#'   iris2 <- droplevels( iris[1:100, ])
#'   f <- randomForest( Sepal.Length ~ . , iris2 )
#'   
#'   emulate( f, iris )

setMethod( 
  'emulate', signature( 'randomForest', 'data.frame' ), 
  function( x, y, default = " __OTHER__" ) {  # default = NA ?
    
    rf_test_names( x, y )
    
    nms.cat <- names( x$forest$xlevels )
    for( nm in names.cat  ) {
    
      allowed <- x$forest$xlevels[[ nm ]]
      if( exists( nm, y ) ) {
        
        # wh <- which( ! y[[ var ]] %in% allowed )
        y[[ nm ]][ ! y[[ nm ]] %in% allowed ] <- default
        
      }
      
    }
    
    return(y)
  
  }
  
)



#' @rdname emulate-methods
#' @aliases emulate,vector,factor-method
#' @examples
#' x <- factor( letters[1:3] )
#' y <- factor( letters[1:6] )
#' 
#' emulate( x, y ) # a    b    c    <NA> <NA> <NA>
#' emulate( y, x ) # adds levels of y
#' emulate( x, y, default="b" )

setMethod( 'emulate', signature( 'factor', 'vector' ), 
  function(x, y, default=NA ) {
    
    y. <- factor( y, levels=levels(x) )
    
    if( ! is.na(default) ) { 
      wh <- ! y. %in% levels(x)
      y.[ wh ] <- default 
    }
     
    return(y.)
  }
)
  

#' @rdname emulate-methods
#' @aliases emulate,vector,character-method
#' @examples
#' x <- letters[1:6] 
#' y <- letters[1:3] 
#' 
#' emulate( x, y )
#' emulate( y, x ) 
#' emulate( x, y, default="b" )

setMethod( 'emulate', signature( 'character', 'vector' ), 
  function(x, y, default=NA ) {
    
    y <- as.character(x)
    
    wh <- ! y %in%  unique(x)
    y[ wh ] <- default 
      
    return(y)
    
  }
)


#' @rdname emulate-methods
#' @aliases emulate,ANY,ANY-method
setMethod( 'emulate', signature( 'ANY', 'ANY' ), function(x,y,...) return(y) )
  
