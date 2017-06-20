#' Make one object emulate another
#' 
#' Make one object more like another by aligning attributes: names, types, 
#' levels
#' 
#' @param x object to change
#' @param template object to emulate
#' @param default character value for missing values
#' @param ... additional parameters 
#' 
#' \code{emulate} makes \code{x} more like a \code{template} so that \code{x} 
#' can be used in places where \code{template} is used. 
#'  
#' This generally means coercing to the type of \code{template} and only 
#' containing values found in \code{template}.
#' 
#' For the \code{randomForest} method, this requires adding a \code{default}
#' 
#' @return 
#'   Returns \code{x} that emulates \code{template}
#'   
#'   The ANY-ANY method simply returns \code{template}
#'   
#' @seealso   
#'   \code{\link{apply.pattern}} 
#'   \code{\link[dp.misc]{conform}}
#'   \code{\link[plyr]{rbind.fill}}
#'   
#' @examples 
#'  # -tk 
#' @export 
#' @docType methods
#' @rdname emulate-methods

setGeneric( 'emulate', function(x, template, ...) standardGeneric('emulate') )

setOldClass( 'randomForest') 

#' @aliases emulate,data.frame,randomForest-method
#' @rdname emulate-methods
#' @examples
#'   iris2 <- droplevels( iris[1:100, ])
#'   iris2$Species 
#'   f <- randomForest( Sepal.Length ~ . , iris2 )
#'   
#'   iris3 <- emulate( iris, f )
#'   predict(f,iris3)

setMethod( 
  'emulate', signature( 'data.frame', 'randomForest' ), 
  function( x, template, default = "Other" ) {  # default = NA ?
    
    rf_test_names( template, x )
    
    nms.cat <- names( template$forest$xlevels )
    for( nm in nms.cat  ) {
    
      allowed <- template$forest$xlevels[[ nm ]]
      if( exists( nm, x ) ) {
        
        # wh <- which( ! x[[ var ]] %in% allowed )
        x[[ nm ]][ ! x[[ nm ]] %in% allowed ] <- default
        
      }
      
    }
    
    return(x)
  
  }
  
)



#' @rdname emulate-methods
#' @aliases emulate,vector,factor-method
#' @examples
#' a <- factor( letters[1:3] )
#' b <- factor( letters[1:6] )
#' 
#' emulate( x=a, template=b )    # adds levels of x
#' 
#' emulate( x=b, template=a )    # a    b    c    <NA> <NA> <NA>
#' emulate( x=b, template=a, default="a" )
#' 

setMethod( 'emulate', signature( 'vector', 'factor' ), 
  function( x, template, default=NA ) {
    
    x. <- factor( x, levels=levels(template) )
    
    if( ! is.na(default) ) { 
      wh <- ! x. %in% levels(template)
      x.[ wh ] <- default 
    }
     
    return(x.)
  }
)
  

#' @rdname emulate-methods
#' @aliases emulate,vector,character-method
#' @examples
#'   a <- letters[1:3] 
#'   b <- letters[1:6] 
#' 
#'   emulate( x=a, template=b ) 
#'   
#'   emulate( x=b, template=a ) 
#'   emulate( x=b, template=a, default="default" )

setMethod( 'emulate', signature( 'character', 'vector' ), 
  function(x, template, default=NA ) {
    
    template <- as.character(template)
    
    wh <- ! x %in%  unique(template)
    x[ wh ] <- default 
      
    return(x)
    
  }
)


#' @rdname emulate-methods
#' @aliases emulate,ANY,ANY-method
setMethod( 'emulate', signature( 'ANY', 'ANY' ), 
           function( x, template,...) {
             warning("Nothing to emulate. No method for ", class(x), " - ", class(template) )
             return(x)
           }
)
  
