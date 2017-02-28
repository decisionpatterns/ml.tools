#' Create labels from breaks 
#' 
#' Create lables from a set of break points
#' 
#' @param breaks numeric; set of break points
#' @param sep string used to seperate high and low poing 
#' 
#' @export

breaks_to_labels <- function(breaks, sep="-") { 
    
  sapply( 1:(length(breaks)-1), function(i) paste0( breaks[[i]], sep, breaks[[i+1]] ) )
  
}


#' @export
#' @import base.tools

semantic_labels <- function(x) { 

  if( length(x) == 1 )
    len <- x else 
    len <- length(x)

  len %>% switch( 
      qw(medium)
    , qw(low,high)
    , qw(low,medium,high) 
    , qw(very_low,low,high,very_high)
    , qw(very_low,low,medium,high,very_high)
    , NULL
  ) 

}  