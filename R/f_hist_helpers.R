#' Data Table j binning functions for continous variables
#'
#' Utilities used for creating various types of bins for 
#' continuous valued variables. These are intended to be used in the \code{j}
#' argument of data.table    
#'  
#' @param x numeric; vector to be summarized using a histogram
#' @param ... additional arguments passed to \code{\link[stats]{quantile}}
#' 
#' @details 
#' 
#' In each case these function produce a a named list of values. 
#' Parameters control the number of elements created.
#' 
#' \code{bin_quantile_breaks_list} gives the quantile break points for a 
#' continuous variable. Unlike histogram features, there is no need to store 
#' break points since the break points themselves become the feature.
#' 
#' @section names:
#' 
#' The names produced by these functions are created by using feature naming
#' conventions [source_variable].[bin].[summary_function]. In latex, this would be 
#' [summary_function]_[source_variable][bin]
#' 
#' Examples:
#' - mpg.[1/5].n 
#' - mpg.[3/7].ν
#' - mpg.[2/4].ρ
#'
#' @return 
#'  all of these functions returns lists; a list with quantiles break. Quantile names are prefixed with 'q' 
#'  for quantile.
#'  
#' @seealso 
#'   \code{\link[stats]{quantile}} which is used to implement 
#' 
#' @examples 
#'
#'   bin_quantile_breaks_list( 1:100 )
#' 
#'   data(mtcars)
#'   bin_quantile_breaks_list( mtcars$mpg )
#'   
#'   mtcars[ by=cyl, , bin_quantile_breaks_list(mpg) ]
#' 
#'        
#' @export

bin_quantile_breaks_list <- function(x, ...) { 
  
  li <- as.list( quantile(x, ...) )
  names(li) <- paste0("q", names(li) %>% gsub( "\\%$", "", . ) )
  
  return(li)
  
}


#' @details 
#' 
#' \code{bin_count_list} divides the range of \code{x} into equal-width buckets 
#' the lowest and highest buckets are then expanded to include -Inf and +Inf 
#' respectively. 
#' 
#' N.B. Using this seperately for test and training sets does not 
#' preserve the break points. It may be necessary to pre-calculate 
#' break-points and apply them afterwards.  
#' 
#' For ts models in which a long history is examined, it could 
#' 
#' @seealso 
#'   \code{dp.misc::segment}
#' 
#' @examples 
#'   
#'   br <- rnorm(100) %>% quantile( probs=seq(0,1,.25) )
#'   rnorm(10) %>% bin_count_list(breaks=br)
#'   
#'   x[ by=Period, , bin_count_list(.N,) ]
#'   mtcars[ by=cyl, , bin_count_list(mpg) ]
#'   
#' @examples 
#'  x <- rnorm(100) 
#'  bin_count_list("x", envir=parent.frame())
#'  bin_count_list(x, envir=parent.frame())
#'   
#'  library(data.table)
#'  data(mtcars)
#'  setDT(mtcars)
#' 
#'  f_hist( mtcars, "mpg", "cyl", bin_count_list )
#' 
#' @import base.tools
#' @export  
 
bin_count_list <- function(x, breaks=5, ..., envir=parent.frame(), labels=names_bin_count ) { 

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

# Feature Name Standards

# names_bin_count <- function(x,n) 
#   paste0(x, ".[", 1:n, "/", n, "].", "n" ) 
# 
# names_bin_freq <- function(x,n)
#   paste0(x, ".[", 1:n, "/", n, "].", nu ) 
# 
# names_bin_density <- function(x,n)
#   paste0(x, ".[", 1:n, "/", n, "].", rho ) 


#' @details 
#' 
#' \code{bin_freq_list} gives the frequencies in the histogram. Frequencies are
#' counts normalized to 1.  It uses \code{bin_count_list}
#' 
#' @examples
#' 
#' rnorm(100) %>% bin_freq_list
#' 
#' mtcars[ , bin_freq_list(mpg) ]
#' mtcars[ , bin_freq_list(mpg), by=cyl ]

bin_freq_list <- function(x, breaks=5, ..., envir=parent.frame(), labels=names_bin_freq ) {

  li <- bin_count_list(x, breaks=breaks, ..., labels=labels, envir=envir)
  tot <- li %>% unlist %>% sum
  li <- lapply(li, function(x) x / tot ) 
  
  return(li)

}


#' @seealso 
#' 
#' \code{hist}'s density values. 
#' 
#' @export

bin_density_list <- function(x, breaks=5, ..., envir=parent.frame(), labels=names_bin_density ) {

  li <- bin_count_list(x, breaks=breaks, ..., labels=labels, envir=envir)
  tot <- li %>% unlist %>% sum
  li <- lapply(li, function(x) 2*x / tot ) 
  
  return(li)

}

#' #' @details 
#' #' 
#' #' \code{bin_density_list} reports \code{hist}'s density values. 
#' #'
#' #' @examples 
#' #' 
#' #'   1:100 %>% bin_density_list
#' #'   
#' #'   library(mtcars)
#' #'   data(mtcars)
#' #'   setDT(mtcars)
#' #'   
#' #'   mtcars[ , bin_density_list("am")]
#' 
#' bin_density_list <- function(x, breaks=5, ..., envir=parent.frame(), labels=names_bin_density ) { 
#' 
#'   if( ! base.tools::is.string(x) ) 
#'     x <- substitute(x)
#'   
#'   x.orig <- x
#'   x <- eval( parse(text=x), envir )
#'     
#'   # Convert breaks to vector
#'   if( length(breaks)==1 ) { 
#'     br <- min(x) + (0:breaks) * diff(range(x))/breaks
#'   } else {
#'     br <- breaks
#'   }
#'   
#'   br[1] <- -Inf
#'   br[ length(br) ] <- +Inf
#' 
#'   cnts <- hist(x, breaks=br, plot=FALSE)$density
#' 
#'   li <- as.list(cnts)
#'   
#'   # NAMES
#'   names(li) <- labels(x.orig, breaks)
#'   
#'   return(li)
#'   
#' }