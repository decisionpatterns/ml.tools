#' Create a histogram feature
#' 
#' Creates a histogram feature from a categorical variable
#' 
#' @param .data data set
#' @param x string; \strong{name} of variable used for histogram
#' @param by character; name of variable(s) used to define the feature grain
#' 
#' @details 
#' 
#' \code{f_hist_cat} creates histogram features from a categorical variable. 
#' This is similar to dummy encoding, but rather than encoding 0 or 1, the count
#' of occurences of the variables are given.
#' 
#' \code{.data} is the data set containing the data; generally this will be a 
#' snapshot or fact table 
#' 
#' \code{x} is the name of the variable used for histogram. \code{x} can only 
#' be a single element character; this may change.
#' 
#' \code{by} is used to group the data prior to performing the histogram.    
#' 
#' NB. 
#' @return 
#' a data object; if \code{by} is provided then that is set as the key  
#'
#' @examples 
#' 
#' data(mtcars)
#' setDT(mtcars)
#' 
#' mtcars %>% f_hist_cat( "gear", "cyl")
#' mtcars %>% f_hist_cat( "gear", c("cyl","am") )
#' 
#' data(iris)
#' setDT(iris)
#' 
#' iris %>% f_hist_cat( "Species" )
#' 
#' # Does not allow multiple x's
#' # for example for join distributions 
#' 
#' # mtcars %>% f_hist_cat( c("gear","am"), "cyl")
#' 
#' @import tidyr
#' @export
 
# To make a proper method, this may have to be an S4 method since behaviour 
# relies on both .data and x
# function( .data, x, by=NULL, fun=bin_count_list , ...)
f_hist_cat <- #  function( .data, x, by=NULL ) UseMethod( 'f_hist' )

.f_hist.cat <- function( .data, x, by=NULL, ... ) { 

  ret <- 
    .data[ by=c(by,x), , .N ] %>% 
    tidyr::spread_(x, "N", fill=0, drop=FALSE, sep=":" )

  if( is.data.table(.data) ) ret %>% setDT
  if( ! is.null(by) ) setkeyv(ret, by)

  return(ret)
}


#' Given a variable name and an environmes
#  @return 
#    list( name=stat )
# .data[ by=by, , fun(x,envir=.SD, ...) ]
# fun: function(x, breaks=5, ..., labels=names_bin_count, envir=parent.frame()) { 

#' @examples 
#' 
#' letters[ runif(100,1,5) %>% floor ] %>% .bin_count_list
#' 
#' data(mpg)
#' setDT(mpg)
#' 
#' fun("model",envir=mpg)
#' mpg$model %>% fun
#' mpg[ , fun(class)]
#' mpg[ by=year, , fun(class)]

.bin_count_list.cat <- function(x, ..., envir=parent.frame() ) { 
  
  if( ! base.tools::is.string(x) ) 
  x <- substitute(x)
  
  x.orig <- x
  x <- eval( parse(text=x), envir )
  
  li <- table(x) %>% as.list
  
  # names(li) <- labels(x.orig, names(li))
  
  return(ret)
}

# f_hist(mpg, "cyl")

#' Internal functions for producing bin list

xbin_count_list <- function(x, ..., envir=parent.frame() ) { 
  
  # HANDLE x as string or set of values
  if( ! base.tools::is.string(x) ) 
    x <- substitute(x)
  
  x.orig <- x
    x <- eval( parse(text=x), envir )
  
  # DISPATCH TO METHODS
  li <- .bin_count_list(x)

  
  # MODES: cnt, freq, density (after they are binned)
  # NAME: 

  
  return(li)
}

.bin_count_list <- function(x, ...) UseMethod('.bin_count_list')

.bin_count_list.factor <- function(x, ...) .bin_count_list.cat(x, ...)

.bin_count_list.character <- function(x, ...) .bin_count_list.cat(x, ...)



.bin_count_list.numeric <- function(x, breaks=5, ..., envir=parent.frame(), labels=names_bin_count ) { 

  if( ! base.tools::is.string(x) ) 
    x <- substitute(x)
  
  x.orig <- x
  x <- eval( parse(text=x), envir )
    
  # Convert breaks to vector
  if( length(breaks)==1 ) { 
    br <- min(x) + (0:breaks) * diff(range(x))/breaks
  } else {
    br <- breaks
  }
  
  br[1] <- -Inf
  br[ length(br) ] <- +Inf

  cnts <- hist(x, breaks=br, plot=FALSE)$counts 

  li <- as.list(cnts)
  
  # NAMES
  names(li) <- labels(x.orig, breaks)

  return(li)
  
}

