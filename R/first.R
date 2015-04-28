#' Get the first, last or nth record of a set of records
#' 
#' @param .data A tbl.
#' @param group_by character; columns thay 
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
#' @export 


# flights %>%
#   filter( ! is.null(tailnum) ) %>%
#   filter( tailnum != '' ) %>%
#   arrange_( c('tailnum', c('year','month','day', 'dep_time') ) ) %>%
#   group_by_( 'tailnum'  )  %>% 
#   slice( n()  ) 


first <- function( .data, group_by, arrange=NULL ) UseMethod('first')

#' @rdname first
#' @import dplyr
#' @export 

first.default <- function( .data, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice(1)

}


#' @examples
#'   first.data.table( flights, 'tailnum', c('year','month','day','dep_time') )

#' @rdname first
#' @import data.table
#' @export 

 first.data.table <- function( dt, key, o ) {
   
   original_key <- key(dt)  
   dt %>% setkeyv( c(key,o) )
  
   ret <- dt[ unique( dt[ , k, with=FALSE ] ), mult="first" ]
   
   dt  %>% setkeyv( original_key )
   
   return(ret)
     
}


# last
# ---------------------------------------------------------

 

last <- function( .data, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice( n() )

}


#' @export 
#' @rdname first 

nth <- function( .data, nth, group_by=NULL, arrange=NULL ) { 

  .data %>%
    dplyr::arrange_( group_by, arrange ) %>%
    dplyr::group_by_( group_by) %>%
    dplyr::slice( nth )

}



#' @examples
#'   first.data.table( flights, 'tailnum', c('year','month','day','dep_time') )

 first.data.table <- function( dt, key, o ) {
   
   original_key <- key(dt)  
   dt %>% setkeyv( c(key,o) )
  
   ret <- dt[ unique( dt[ , k, with=FALSE ] ), mult="first" ]
   
   dt  %>% setkeyv( original_key )
   
   return(ret)
     
}
