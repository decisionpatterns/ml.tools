#' stitch two feature data tables together
#' 
#' stitches two data.tables together 
#' 
#' @param x data.table; 
#' @param y data.table;
#' 
#' \code{stitch} merges (joins) \code{x} to \code{y} using a LEFT INNER JOIN. Special 
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
#' 
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
#' @seealso \code{\link[data.table]{merge}}
#' 
#' @rdname
#' @export 

stitch <- function(x,y) { 

  # STITCH BASES ON RELATIONSHIP BETWEEN y AND x
  
  if( all( key(y) %in% names(x) ) ) {
    
    # CASE 1: MANY-to-1 -or- 1-TO-1 MERGE 
    #   A LEFT MERGE IS PERFORMED AFTER DEDUPING Y
    x. <- .stitch.straight(x,y)

  } else if( any( key(y) %in% names(x) ) ) { 
    
    # CASE 2: 1-TO-MANY MERGE
    #   PIVOT UNMATCHED KEYS ARE AVAILABLE IN X THESE ARE PIVOTED OUT 
    x. <- .stitch.outer(x,y)
    
  } else { 
    
    stop( "There are no keys of y found in x.")
    
  }
  
  # CASE 3: more keys in y than in x

  
} 


#' \code{stitch.outer} is similar to/identical .stitch.straight
#' @examples
#'   cars <- mtcars
#'   cars$model <- rownames(mtcars)
#'   setDT(cars)
#'   setkey( cars, model )
#'   x <- cars[ 1:10, list(model, mpg ) ]   
#'   setkey( x, model ) 
#'   y <- cars[ c(1:5,1:5), list(model, cyl, hp, drat, wt ) ]
#'   setkey( y, model, cyl )
#'   setkey( x, model)
#'   .stitch.outer(x,y)
#'   x <- mtcars
#'   setDT(x)
#'   key(x)
#' 
#' x <- data.table( customer=letters[1:4] )
#' setkey( x, customer )
#' 
#' y <- data.table( 
#'      customer = sort( letters[ c(1:4,1:4,1:4) ] )
#'    , time = 1:3 
#'    , income = round(rnorm(12,10) * 10) 
#'    , expense = round( rnorm(12,10) )
#' )
#' setkey( y, customer, time )
#' 
#' .stitch.outer(x,y)

.stitch.outer <- function(x,y) {
  
  require( reshape2, quietly=TRUE )
  require( formula.tools, quietly=TRUE )
  
  if( ! any( key(y) %in% names(x) ) ) {
     stop( "There are no keys of y found in x.")
  }
  
  #' GET THE LIST OF VARIABLES IMPORTANT TO THE MERGE
  Ys <- key(y)
  Xs <- if( ! is.null(key(x)) ) key(x) else names(x)   
  
  id.vars      <- intersect( Ys, Xs )          # x,y merge key(s)
  id.vars.vary <- setdiff( Ys, id.vars  )  # y keys not in that.  
  measures     <- setdiff( names(y), key(y) ) 
  
  # DETECT AND ALERT ON CARDINALITY OF PIVOTS
  
  # Melt 
  # id.vars : matching columns 
  # measure.vars = pi o
  y.me <- melt( 
      data = y 
    , na.rm = TRUE
    , id.vars = c(id.vars, id.vars.vary ), #release_dt, release_wk,  measure_wk, W),  
    , measure.vars = measures
  )
    
  #' LHS := key.x_1 + key.x_2 + ...  
  #' RHS := 
  form <- LHS ~ RHS 
  
  lhs( form ) <- 
    parse( text=paste( id.vars, collapse="+" ) )[[1]] 
  
  rhs( form ) <-  
    parse( text=paste( c( 'variable', id.vars.vary ), collapse="+" ) )[[1]]
    
  
  # DUPLICATES.
  #  DUPLICATES ARE AUTOMATICALLY HANDLED BY THE 
  y. <-  dcast.data.table( y.me, form , fun.aggregate=mean, na.rm=TRUE ) 
  
  
  # DUP.ACTION
  y. <- dedup(y.)

  # MERGE
  merge( x, y., by=key(x), all.x=TRUE )
  
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
#'   x <- cars[ 1:20, list(model, mpg, cyl) ]    
#'   y <- cars[ 1:10, list(model, hp, drat, wt ) ]
#'   
#'   # straight, no duplicates in y.
#'   .stitch.straight( x, y )
#'   
#'   c3 <- rbind2(y,y)  
#'   setkey( c3, model)
#'   attr( c3, "dup.action") <- dup.last
#'   .stitch.straight( x, c3 ) 
#'   
#'   
#'   # Provide dup.action
#'   attr( c3, "dup.action" ) <- function(x) x[ ! duplicated(x), ]
#'   .stitch.straight( x, c3 )
#'   
#'   # Ineffective dup.action 
#'   attr( c3, "dup.action" ) <- identity
#'   expect_error( .stitch.straight( x, c3 ) )
#'   
#'   .stitch.straight( x, y )
#' @rdname stitch     

.stitch.straight <- function(x,y) { 
  
  if( ! identical(sort(key(x)), sort(key(y)) ) )
    stop( "Keys of x and y don't match." )

  # DEDUPLICATE.  DUP.ACTION
  y <- dedup(y)

  # MERGE
  merge( x, y, by=key(x), all.x=TRUE )

}    


#' dedup records, preserving attributes
dedup <- function(y) {
  
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
