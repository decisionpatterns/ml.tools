#' stitch two feature data tables together
#' 
#' stitches two data.tables together 
#' 
#' @param x data.table; 
#' @param y data.table;
#' 
#' \code{stitch} merges (joins) two data frames by a LEFT JOIN with special 
#' behaviours according to indexes of \code{x} and \code{y}.  
#' 
#' 
#' If there are more than one \code{y} for every x. Duplicate rows are \code{y} is pivoted until 
#' 
#' If the indexes of \code{x} and \code{y} are the same, \code{y} is assumed to
#' be additional attributes for x
#' 
#' \strong{imputation}
#' 
#' 
#' 
#' @return data.table; X and Y appropriately merged/joined.
#' 
#' @note 
#'   Indexes may not be necessary and this may work by matching names between 
#'   \code{x} and \code{y}
#' 
#' 
#' @seealso \code{\link[data.table]{merge}}
#' 
#' 

stitch <- function(x,y) { 

  # STITCH BASES ON RELATIONSHIP BETWEEN y AND x
  
  if( all( key(y) %in% names(x) ) {
    
    # CASE 1: MANY-to-1 -or- 1-TO-1 MERGE 
    #   A LEFT MERGE IS PERFORMED AFTER DEDUPING Y
    x. <- .stitch.straight(x,y)

  } else if( all( key(y) %in% key(x) ) ) { 
    
    # CASE 2: 1-TO-MANY MERGE
    #   PIVOT UNMATCHED KEYS ARE AVAILABLE IN X THESE ARE PIVOTED OUT 
    x. <- .stitch.outer(x,y)
    
  }
  
  # CASE 3: more keys in y than in x

  
} 


#' \code{.stitch.straight} performs a LEFT merge/join on two data.tables when \code{x} and \code{y} have
#' the same keys/the keys of y are all in the 
#' 
#' @examples
#'   data(mtcars)
#'   cars <- mtcars
#'   cars$model <- rownames(mtcars)
#'   setDT(cars)
#'   setkey( cars, model )
#'   c1 <- cars[ 1:20, list(model, mpg, cyl) ]    
#'   c2 <- cars[ 1:10, list(model, hp, drat, wt ) ]
#'   
#'   # straight, no duplicates in y.
#'   expect_is( .stitch.straight( c1, c2 ), 'data.table' )
#'   
#'   c3 <- rbind2(c2,c2)
#'   setkey( c3, model)
#'   expect_error( .stitch.straight( c1, c3 ) )
#'   
#'   # Provide dup.action
#'   attr( c3, "dup.action" ) <- function(x) x[ ! duplicated(x), ]
#'   expect_is( .stitch.straight( c1, c3 ), 'data.table' )
#'   
#'   # Ineffective dup.action 
#'   attr( c3, "dup.action" ) <- identity
#'   expect_error( .stitch.straight( c1, c3 ) )
#'   
#'   .stitch.straight( c1, c2 )
#' @rdname stitch     

.stitch.straight <- function(x,y) { 
  
  if( ! identical(sort(key(x)), sort(key(y)) ) )
    stop( "Keys of x and y don't match." )

  # DUP.ACTION
  y <- .dedup(y)
#   # DUP.ACTION
#   if( any( duplicated(y) ) )
#     if( ! is.null( attr( y, "dup.action" ) ) ) { 
#       dup.action <- attr( y, "dup.action" )
#       y <- dup.action(y)      
#     } else {
#       stop( "Duplicates detected in y with no dup.action" )
#     }
#  
#   if( any( duplicated(y)) )
#     stop( "dup.action did not eliminate all the duplicates" )
#   
  # MERGE
  merge( x, y, by=key(x), all.x=TRUE )

}    

#' \code{stitch.under} is similar to/identical .stitch.straight
#' @examples
#'   .stitch.under(x,y)
.stitch.under <- function(x,y) {
  
  if( ! all( key(y) %in% key(x) ) ) {
    stop( "Not all keys of y are in x")
  }
  
  # DUP.ACTION
  y <- .dedup(y)

  # MERGE
  merge( x, y, by=key(x), all.x=TRUE )
  
}  



#' dedup records
.dedup <- function(y) {
  
  # DUP.ACTION
  if( any( duplicated(y) ) )
    if( ! is.null( attr( y, "dup.action" ) ) ) { 
      dup.action <- attr( y, "dup.action" )
      y <- dup.action(y)      
    } else {
      stop( "Duplicates detected in y with no dup.action" )
    }
 
  if( any( duplicated(y)) )
    stop( "dup.action did not eliminate all the duplicates" )
  
  return(y)
  
}
