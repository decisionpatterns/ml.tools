#' stitch two feature data tables together
#' 
#' stitches two data.tables together based on (shared) keys
#' 
#' @param x data.table; 
#' @param y data.table;
#' @param ... additional arguments passed to subsequent methods
#' 
#' \code{stitch} does two things.  First, it ensures removes duplicates from 
#' \code{y}. This is done using \code{dup.action}. 
#' 
#' merges (joins) \code{x} to \code{y} using a LEFT INNER JOIN. Special 
#' behaviours make it so this according to indexes/columns of \code{x} and \code{y}.  
#' 
#' If there are more than one \code{y} for every x. Duplicate rows of \code{y} 
#' are pivoted until a one-to-one or many-to-one relationship exists, allowing 
#' for a STRAIGHT JOIN.
#' 
#' If all indices of \code{y} are found in \code{x}, \code{y} are DEDUPED and 
#' merged via a STRIAGHT JOIN to \code{x} via the indices of \code{y}. 
#' 
#' If there are more 
#' 
#' DEDUPLICATIONS are handled via the DEDUP action which defaults to attr( )
#' be additional attributes for x
#' 
#' \strong{imputation} is 
#' 
#' @return data.table; X and Y appropriately merged/joined.
#' 
#' @note 
#'   Indexes may not be necessary and this may work by matching names between 
#'   \code{x} and \code{y}. 
#'   
#'   LHS := Y's %intersect% X's
#'   RHS := Y's \ X's
#' 
#' @seealso 
#'   \code{\link[data.table]{merge}}
#'   \code{dup.pivot} in the \code{dup.actions} package.
#' 
#' @import dup.actions data.table
#' @rdname stitch
#' @export 

stitch <- function(x,y, ...) { 

  # STITCH BASES ON RELATIONSHIP BETWEEN y AND x
  
  if( all( key(y) %in% names(x) ) ) {
    
    # CASE 1: MANY-to-1 -or- 1-TO-1 MERGE 
    #   A LEFT MERGE IS PERFORMED AFTER DEDUPING Y
    x. <- .stitch.straight(x,y, ...)

  } else if( any( key(y) %in% names(x) ) ) { 
    
    # CASE 2: 1-TO-MANY MERGE
    #   PIVOT UNMATCHED KEYS ARE AVAILABLE IN X THESE ARE PIVOTED OUT 
    x. <- .stitch.outer(x,y, ...)
    
  } else { 
    
    stop( "There are no keys of y found in x. No stitching can be done.")
    
  }
  
  # CASE 3: more keys in y than in x
  
  return(x.)
  
} 


#' \code{stitch.outer} is similar to/identical .stitch.straight
#' @examples
#' 
#'   x <- data.table( customer=letters[1:4] )
#'   setkey( x, customer )
#' 
#'   y <- data.table( 
#'        customer = sort( letters[ c(1:4,1:4,1:4) ] )
#'      , time     = 1:3 
#'      , income   = round(rnorm(12,10) * 10) 
#'      , expense  = -round( rnorm(12,10) )
#'      , count    = 1
#'   )
#' 
#'   y <- rbind(y, y[c(1,1,2),] )
#'   y <- y[ -(10:11), ]
#'   y[ 8, income := NA]
#'   setkey( y, customer, time )
#' 
#'   ml.tools:::.stitch.outer(x,y)
#'   
#' @import dup.actions
#' @rdname stitch

.stitch.outer <- function(x,y) {
  
  require( reshape2, quietly=TRUE )
  require( formula.tools, quietly=TRUE )
  
  if( ! any( key(y) %in% names(x) ) ) {
     stop( "There are no keys of y found in x.")
  }
  
  #' GET THE LIST OF VARIABLES IMPORTANT TO THE MERGE
  key_y <- key(y)
  key_x <- if( ! is.null(key(x)) ) key(x) else names(x)   
  
  id.vars      <- intersect( key_y, key_x )          # x,y merge key(s)
  vary <- setdiff( key_y, id.vars  )  # y keys not in that.  
  measures     <- setdiff( names(y), key(y) ) 
  
  # DETECT AND ALERT ON CARDINALITY OF PIVOTS

  y. <- dup.pivot( y, id.vars, vary )

  # DUP.ACTION
  # y. <- dedup(y.)   

  # MERGE
  x. <- merge( x, y., by=key(y.), all.x=TRUE )
  
  return(x.)
  
}  


#' \code{.stitch.straight} performs a LEFT merge/join on two data.tables when \code{x} and \code{y} have
#' the key of \code{y} are all in the x
#' 
#' @examples
#'   x <- data.table( 
#'        customer = sort( letters[ c(1:4,1:4,1:4) ] )
#'      , date     = 1:3
#'      , income   = round(rnorm(12,10) * 10) 
#'      , expense  = -round( rnorm(12,10) )
#'      , count    = 1
#'   )
#' 
#'   # DUPLICATES
#'   y <- data.table( customer=letters[1:2], first_name=c('Foo', 'Bar') )
#'   setkey( y, customer )
#' 
#'   ml.tools:::.stitch.straight( x, y  )
#' 
#' @import dup.actions
#' @rdname stitch     

.stitch.straight <- function(x,y, ...) { 
  
  if( ! all( key(y) %in% names(x) ) )
    stop( "Keys of x and y don't match." )

  # DEDUPLICATE.  DUP.ACTION
  y. <- dedup(y, ...)

  # MERGE
  x. <- merge( x, y., by=key(y.), all.x=TRUE )

  return(x.)
  
}    

