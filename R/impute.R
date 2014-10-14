# --------------------------------------------------------------------------
# FUNCTION: impute
#  Takes a data.frame and imputes columns with missing data and
#  the records an attribute "imputes" on the data frame that 
#  holds the constants that get imputed.
#
#  Those imputed values are then saved back to the data.frame as an attribute.
#
# --------------------------------------------------------------------------

#' Impute missing values
#' 
#' Imputes missing values for an object, \code{x}
#'
#' @param x object
#' @param fun function. 
#' @param ... other arguments passed to fun
#' @param na.rm boolean. Whether to remove \code{NA}s.  (Default:TRUE)
#'
#' Imputes missing values through using a flexible function \code{fun}.
#' 
#' \code{impute.data.table} skips imputing columns where all value are \code{NA} 
#' 
#' @return 
#' An object with the same class as \code{x} with missing values replaced by
#' the most commonly occuring value(s), i.e. the mode for numeric values.  When
#' there is a tie for multiple values, the replacement is sampled from these.   
#'    
#' @examples
#' x <- c( 1, NA, 3:5 )
#' impute( x, mean)   
#' 
#' x <- as.factor(mtcars$cyl )
#' x[1:4] <- NA
#'
#' @rdname impute
#' @export

impute <- function(x, fun, ... ) UseMethod( "impute" )



#' @rdname impute
#' @export

impute.default <- function(x, fun, ...) {
  counts <- table(x)
  most_freqs <- names(counts)[ counts==max(counts) ]
  x[ is.na(x) ] <- sample( most_freqs , size=sum( is.na(x)), replace=TRUE)
  return(x)
} 


#' @export
#' @rdname impute

impute.logical <- function( x, fun, ... ) 
  as.logical( impute.default(x) )


  
#' @rdname impute
#' @export 

impute.numeric <- function(x, fun=mean, ... ) {
  x[ is.na(x) ] <- fun(x, na.rm=TRUE, ...  )
  return(x)
}


#' @export
#' @rdname impute

impute.integer <- function(x, fun=median, ..., na.rm=TRUE ) {
  x[ is.na(x) ] <- fun(x, na.rm=TRUE  )
  return(x) 
}
  

#' @export
#' @rdname impute

impute.character <- function( x ) {
  counts <- table(x)
  most_freqs <- names(counts)[ counts==max(counts) ]
  x[ is.na(x) ] <- sample( most_freqs , size=sum( is.na(x)), replace=TRUE)
  return(x)
}

#' @export
#' @rdname impute

impute.factor <- function( x ) {
  counts <- table(x)
  most_freqs <- names(counts)[ counts==max(counts) ]
  x[ is.na(x) ] <- sample( most_freqs , size=sum( is.na(x)), replace=TRUE)
  return(x)
}


#' @rdname impute
#' @export 


impute.POSIXct <- function(x, fun=mean, ..., na.rm=TRUE) {
  x[ is.na(x) ] <- fun( x, na.rm=na.rm )
  return(x)  
}



#' @export
#' @rdname impute

impute.data.frame <- function(x, fun, ... ) {
  
  for( nm in names(x) ) {
    # cat( "Imputing", nm, "with class", class(x[[nm]]), "\n" )
    x[, nm ] <- impute( x[,nm] )
  }
  
  return(x)
  
}




#' @export
#' @rdname impute

impute.data.table <- function(x, fun, ... ) {
  
  # Impute for each column skipping those will all NA values
  for( nm in names(x) ) {
    # cat(nm, "\n")
    if( all( is.na( x[[nm]] ) )) next 
    set( x, , nm, impute( x[[nm]] ) )
  } 
    
  return(x)

}
