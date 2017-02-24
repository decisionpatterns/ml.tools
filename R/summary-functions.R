#' Special summary function for use with f_bin
#' 
#' Summarization functions for the bin-split-apply-combine-spread pattern
#' 
#' @param .data data object with \code{\.bin} column used for summarization 
#' @param x character; name of values to be summarized
#' @param by character; names of columns that define the grain of feature
#' @param ... unused.
#' 
#' @details 
#' 
#' Thess functions take a table object with a \code{.bin} column. They each use 
#' that \code{.bin} for further summarization.
#' 
#' Each produced long-format table with one column for each \code{by} value,  
#' \code{.bin} and \code{value}. 
#' 
#' @return 
#' 
#' A table object grouped by \code{by} and \code{,bin} and a \code{value} 
#' column that is the summary of the transformation
#' 
#' @examples 
#' 
#' mpg %>% mutate(.bin=cut(cty,5)) %>% .cnt( 'cty', by=c('year','cyl')) %>% 
#'   spread( .bin, value )
#' 
#' @rdname summary
#' @export

.cnt <- function(.data, x=NULL, by, ...) { 
  .data %>% count_( c(by, '.bin') ) %>% rename( value = n ) %>% 
    structure( summary="cnt" ) 
}

#' @examples 
#' 
#' mpg %>% mutate(.bin=cut(cty,5)) %>% .freq( by=c("year","trans") )
#' 
#' @rdname summary
#' @export

.freq <- function(.data,x=NULL, by, ...) 
   .data %>% count_( c(by,'.bin') ) %>% 
    mutate( value = (n / sum(n)) %>% round(3))  # %>% sticky %>% structure( suffix="n")


#' @param x 
#' 
#' @references 
#' 
#' See dplyr's 
#' \url[NSE]{https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html} 
#' vignette for how to implement summary functions like sum that sum on \code{x}.
#' This implements both 'Setting Variable Names' and 'Mixing Constants and Variables'
#' 
#' @example 
#'
#'   mpg %>% mutate(.bin=cut(cty,5)) %>% .sum( x="cty", by=c("year","trans") )
#'   
#' @rdname summary
#' @export

.sum <- function( .data, x, by, ...)
  .data %>% group_by_( .dots=c(by,'.bin') ) %>% 
      summarise_( .dots = 
        list( interp( ~ sum(x), x=as.name(x) ) ) %>% setNames('value')   
      ) 
  
#' @details 
#' 
#' \code{.cumsum} is implemented by first calculating \code{.sum}, followed by 
#' calculating \code{cumsum} for each \code{by}
#'   
#' @example 
#'
#'   mpg %>% mutate(.bin=cut(cty,5)) %>% .cumsum( x="cty", by=c("year","cyl") ) %>% 
#'     spread( .bin, value )
#'   
#' @rdname summary
#' @export

.cumsum <- function(.data, x, by, ...) {
  .data %>% 
    .sum( x=x, by=by ) %>% 
    arrange_( .dots=c(by,'.bin') )  %>% 
    complete_( cols=c(by, '.bin'), fill=list(value=0)) %>%    # Needs complete
    group_by_( .dots=c(by) ) %>%  
    mutate( value=base::cumsum(value) )
}  


# MANUAL WORKING EXAMPLE:
#
# mpg %>%  mutate(.bin=cut(cty,5)) %>%
#   .sum( x="cty", by=c("year","cyl") ) %>%
#   arrange_( .dots=qw(year,cyl,.bin))  %>%
#   complete_( cols=qw(year,cyl,.bin), fill=list(value=0)) %>%
#   group_by_(.dots=qw(year,cyl) ) %>%
#   mutate( value=base::cumsum(value)) %>% 
# 
#     spread( .bin, value )

