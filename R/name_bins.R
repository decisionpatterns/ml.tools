#' Create/update names for bins
#' 
#' Create/update names for bins
#' 
#' @param bin_names character; bin names
#' @param label function applied to \code{bin_names}
#' @param ... currently unused 
#' @param sep = "."
#' @param src string; name of the source variable 
#' @param prefix string; prefix
#' @param suffix string; suffix
#' 
#' @details 
#' 
#' [prefix].x.label(bin_names)](.suffix)
#' 
#' @examples 
#' 
#' name_bins( 1:3 )
#' name_bins( 1:3, . %>% paste0( "/3" ) )
#' name_bins( 1:3, . %>% paste0( "/3" ) %>% base.tools::parenthesize, src="x", suffix=rho )
#'  
#' @export

name_bins <- function(bin_names, label=identity, ..., sep=".", src=NULL, prefix=NULL, suffix=NULL ) {

  nms <- label(bin_names)
  if( ! is.null(src) )    nms <- paste(src, nms, sep=sep ) 
  if( ! is.null(prefix) ) nms <- paste(prefix, nms, sep=sep )
  if( ! is.null(suffix) ) nms <- paste(nms, suffix, sep=sep )
  
  return(nms)
  
}




# names_bin_count <- function(x,n) 
#   paste0(x, ".[", 1:n, "/", n, "].", "n" ) 
# 
# names_bin_freq <- function(x,n)
#   paste0(x, ".[", 1:n, "/", n, "].", nu ) 
# 
# names_bin_density <- function(x,n)
#   paste0(x, ".[", 1:n, "/", n, "].", rho ) 