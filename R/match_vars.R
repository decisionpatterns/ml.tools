#' Add dummy columns to variables data set
#' 
#' Adds dummy columns to the first data frame that are present in the second
#'
#' @param data1 data set to which columns are added. 
#' @param data2 data set to get the columns are taken.
#' 
#' \code{match_vars} adds columns to \code{data1} that are unmatched from 
#' \code{data2}.  The columns added are given defaults \code{FALSE} for PA 
#' variables, \code{0} for VAL variables and \code{0} for ERR variables.
#' 
#' @return a data set like \code{data1} with unmatched columns of \code{data2}
#' added.  These columns are given deefault values.
#' 
#' @seealso 
#'   \code{\link{match_features}}
#'   \code{\link[plyr]{rbind.fill}}
#' @export

match_vars <- function( data1, data2 ) { 
  
  for( nm in setdiff( names(data2), names(data1) ) ) {
    
    if( grepl( '_PA$', nm )  )
      data1[[nm]] <- FALSE
    
    if( grepl( '_VAL', nm) ) 
      data1[[nm]] <- 0
    
    if( grepl( '_ERR', nm) ) 
      data1[[nm]] <- 0
    
  }
  
  return(data1)
  
}


#' Add dummy columns to variables data set
#' 
#' Adds dummy columns to the first data frame that are present in the second
#'
#' @param data1 data set to which columns are added. 
#' @param data2 data set to get the columns are taken.
#' 
#' \code{match_vars} adds columns to \code{data1} that are unmatched from 
#' \code{data2}.  The columns added are given defaults \code{FALSE} for PA 
#' variables, \code{0} for VAL variables and \code{0} for ERR variables.
#' 
#' @return a data set like \code{data1} with unmatched columns of \code{data2}
#' added.  These columns are given deefault values.
#' 
#' @seealso \code{\link{match_features}}
#' 
#' @rdname match_features
#' @export

match_features <- function( data1, data2 ) { 
  
  for( nm in setdiff( names(data2), names(data1) ) ) {
    
    if( grepl( '^PA_', nm )  )
      data1[[nm]] <- FALSE else 
        data1[[nm]] <- 0
    
  }
  
  return(data1)
  
}
