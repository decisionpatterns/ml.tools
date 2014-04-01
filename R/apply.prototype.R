#; apply.prototype 
#' 
#' Applies a prototype to data in a higly efficient way in order to
#' prepare the data for soc 
#' @export
apply.prototype <- function( data, proto, default="__OTHER__" ) { 

  # MARCH COLUMNS ON NAME
  dat <- data[ , names(proto), with=FALSE ]

  # IDENTIFY FACTORS 
  #  - Restrict values in data to prototype levels 
  facs <- names(proto)[ sapply(proto, is.factor) ]
  
  for( fac in facs ) 
    dat[[ fac ]][ ! dat[[ fac ]] %in% levels( proto[[ fac ]] ) ]  <- default

  # Drop unused levesl 
  # dat <- droplevels(dat)

  # RBIND TO ADD LEVELS
  #  - add an NA row to proto to ensure that levels are preserved
  #  - rbind the two sets
  #  - remove added row
  proto. <- proto[nrow(proto)+1,]  # ensure this is all NAs
  dat <- rbind( proto., dat ) 
  
  dat[ -1, ]  # REMOVE NAs row
  
}
