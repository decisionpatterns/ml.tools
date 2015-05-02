#' Get the first, last or nth record of a set of records
#' 
#' @param .data A tbl.
#' @param group_by character; columns that list the group_bys
#' @param arrange character; ordering columns
#'  
#' @examples
#'   library(nycflights13)    
#'   
#'   flights  %>% first( 'tailnum', c('year','month','day', 'dep_time') )
#'   flights  %>% last( 'tailnum', c('year','month','day', 'dep_time') )
#'   flights  %>% nth( 2, 'tailnum', c('year','month','day', 'dep_time') )
#'   
#'   first.data.table( flights, 'tailnum', c('year','month','day','dep_time') )
#'   
#' @import dplyr 
# @import data.table
#' @export 


first <- function( .data, group_by, arrange=NULL ) UseMethod('first')


#' @rdname first
#' @export 

first.default <- function( .data, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice(1)

}


#' @rdname first
#' @export 

 first.data.table <- function( .data, group_by=key(.data), arrange ) {
   
   original_key <- key(.data)  
   .data %>% setkeyv( c(group_by,arrange) )
  
   ret <- .data[ unique( .data[ , group_by, with=FALSE ] ), mult="first" ]
   
   .data  %>% setkeyv( original_key )
   
   return(ret)
     
}



# last
# ---------------------------------------------------------

#' @export 
#' @rdname first 

last <- function( .data, group_by, arrange=NULL ) UseMethod('last')

#' @export 
#' @rdname first 

last.default <- function( .data, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice( n() )

}

#' @rdname first
#' @export 

last.data.table <- function( .data, group_by=key(.data), arrange ) {
   
   original_key <- key(.data)  
   .data %>% setkeyv( c(group_by,arrange) )
  
   ret <- .data[ unique( .data[ , group_by, with=FALSE ] ), mult="last" ]
   
   .data  %>% setkeyv( original_key )
   
   return(ret)
     
}



#' @export 
#' @rdname first 

nth <- function( .data, nth, group_by=NULL, arrange=NULL ) UseMethod('nth') 
  
nth.default <- function( .data, nth, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice( nth )

}
