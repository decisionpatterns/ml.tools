#' apply.pattern 
#' 
#' Applies a pattern to data in to make it closely resemble another data set. 
#' 
#' @param data data
#' 
#' @param pattern pattern; data object with zero or more rows that contain 
#' metadata about the object types in the form of attributes.
#' 
#' @param default; value to be used when levels of the data are are not found 
#' in a factor. The default is to use the first element given by \code{levels}. 
#' See Details.
#' 
#' Two operations are performed:
#' 
#' Columns are reordered to match the pattern.
#' 
#' Factors are coerced to have the same levels as pattern, setting the default 
#' if the don't match.  
#' 
#' @section Factor levels:
#' 
#' The \code{default} can be a numeric, character or function. 
#' 
#' If \code{numeric} this is taken an element in the levels of the factor. 
#' By convention, the default level is the first of a factors.  This is also 
#' often, but not always, the most commonly occuring value. 
#' 
#' If \code{character} then this value is used as the level. Since, 
#' this will be the same for each factor column, care must be taken so that all 
#' columns have the same default value. This is useful for when new values may
#' arise.
#' 
#' If \code{default} is a function, it is applied to the vector of levels for 
#' the vector in \strong{pattern}. This allows selecting the LAST level by using 
#' \code{length}
#' 
#' 
#' @note
#' 
#' Using zero-row data objects for patterns is nice because they preserve 
#' level attributes through subset and other operations. 
#' 
#' 
#' @seealso
#'   \code{\link{emulate}} \cr
#'   \code{\link[plyr]{rbind.fill}} \cr
#'   \code{bind_rows} from dplyr \cr
#'   \code{most_freq} from the \code{cardinality} package
#'
#' @examples
#' 
#'  pattern <- data.frame( 
#'      letters=as.factor(letters[1:3])
#'    , LETTERS=as.factor(LETTERS[1:3]) 
#'    , numbers=1:3L
#'    , char=letters[1:3]
#'  )  
#'  
#'  data <- data.frame( 
#'      letters=as.factor(letters[1:4])
#'    , LETTERS=as.factor(LETTERS[1:4])
#'    , numbers=1:4L
#'    , char=LETTERS[1:4] 
#'  ) 
#'        
#'  apply.pattern( data, pattern )          # default first entry 
#'  apply.pattern( data, pattern, 2 )       # default second entry
#'  apply.pattern( data, pattern, length )  # default last entry 
#'  apply.pattern( data, pattern, function(x) length(x) -1  )  # second to last
#'
#' @import data.table  
#' @export

apply.pattern <- function( data, pattern, default=1 ) { 

  # MARCH COLUMNS ON NAME
  setDT(data)
  dat <- data[ , names(pattern), with=FALSE ]

  # IDENTIFY FACTORS 
  #  - Restrict values in data to pattern levels 
  facs <- names(pattern)[ sapply(pattern, is.factor) ]
  
  # Loop over factors, changing items of data that do not match the 
  # pattern's levels
  for( fac in facs ) {
    
    value <- if( is.function(default) ) default( levels(pattern[[fac]]) ) else default
    
    dat[[fac]][ ! dat[[fac]] %in% levels( pattern[[fac]] ) ] <-     
      if( is.numeric(value) ) 
        levels(pattern[[fac]])[[value]] else        
        value
  }
  
  # Drop unused levesl 
  # dat <- droplevels(dat)

  # RBIND TO ADD LEVELS
  #  - add an NA row to pattern to ensure that levels are preserved
  #  - rbind the two sets
  #  - remove added row
  pattern. <- pattern[nrow(pattern)+1,]  # ensure this is all NAs
  dat <- rbind( pattern., dat ) 
  
  dat[ -1, ]  # REMOVE NAs row
  
}
