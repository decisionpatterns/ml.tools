#' Generate features at the correct grain.
#' 
#' bin-split-apply-combine-spread
#' 
#' @param .data table structure
#' @param x string; name of variable to summarize
#' @param by character; name of variables to group by.
#' @param bin function; used to bin \code{x}. See Details.
#' @param summary function; used to summarize after aggregation. See Details.
#' @param fill numeric; values used to fill in missing values.
#' @param ... arguments passed to additional functions
#' @param suffix character; values to add as a suffix to variable names. See Details.
#' 
#' @details 
#' 
#' This function summarized values at the correct level.  
#' 
#' \code{bin} should be a function that takes x and returns a factor with the 
#' same length as \code{x}. The levels should indicate bins of \code{x}. Useful
#' functions for categorical variables are `cut` and `cut_n`. For categorical 
#' `fct_lump`, `lump` and `lump_smart` work well.
#' 
#' 
#' @section Background:
#'  
#' This an extension of the split-apply-combine pattern : 
#' bin-(split-apply-combine)-spread where multiple of these steps are linked
#' 
#' This is the split-apply-combine pattern 
#'
#' 1. create `.bin` variable of `x` using `lump` (categorical) or `cut` (continous) 
#'    
#' 2. Create long summary by grouping on `by` and `.bin`. This is the split-apply-combine pattern
#'    The  resulting summary should have a single `value` for each unique `by` and `.bin` 
#'
#' 3. Combine the results by `spread` or `dcast` .bin to the column headers; 
#'    there should be 1 remaining value for each cell. Other cells should be 
#'    filled by a value appropriate to the metrics 
#'        
#' 4. standardize names: 
#' 
#' @examples 
#' 
#'  data(mpg)
#'  mpg %>% f_bin("cty")         # No (group_)by
#'  mpg %>% f_bin("cty", "year")  
#'  mpg %>% f_bin("cty", c("year", "cyl") )
#'  mpg %>% f_bin("cty", c("year", "cyl"), summary=.cnt )
#' 
#'  mpg %>% f_bin("cty", summary=.freq ) 
#'  mpg %>% f_bin("cty", c("year", "cyl"), summary=.freq )
#'  
#'  mpg %>% f_bin("cty", c("year", "cyl"), summary=.sum )
#'  mpg %>% f_bin("cty", c("year", "drv"), summary=.cumsum )
#'  
#'  # Categorical Variables:
#'  mpg %>% f_bin("drv", c('year','cyl'), bin=identity, summary=.cnt )
#'  
#'  mpg$trans <- mpg$trans %>% as.factor
#'  mpg %>% f_bin("trans", c('year','cyl'), bin=pryr::partial( forcats::fct_lump, n=6 ), summary=.cnt )
#'  
#'  # 
#'  
#' @notes 
#'
#' The choice ot summary function is related to 
#' 
#' @import dplyr tidyr lazyeval
#' @export 

f_bin <- function( .data           # snapshot or fact data
                   , x             # string
                   , by=NULL       # ? getOption('response.grain') #' @param 
                   , bin = cut_n   #' binning function 
                   , summary = .cnt #' summary function
                   , fill    = 0   #
                   , ...           #' arguments passed to bin,  summary, etc. 
                   , suffix  = NULL 
) { 
 
  # CREATE: .bin variable for splitting and summarizing x for bin identities
  ret <- 
    .data %>%
    
     # BIN 
     # Uses 
     # - lazyeval::interp to mix quoted and unquoted arguments 
     # - dots to navigate around NSE
     # - setNames to setNames
     mutate_( .dots = setNames( list( interp( ~ bin(x), x=as.name("cty") ) ), ".bin" ) )  %>% 
    
     # SPLIT-APPLY-COMBINE: Summarize at the correct grain 
     group_by_(.dots = c(by)) %>% 
     summary(x,by)            %>%        # See summary argument
  
     # PIVOT/SPREAD: Leave at correct grain  
     select_(.dots=c(by,'.bin', 'value') ) %>%
     spread_( '.bin', value='value', fill=fill ) 
  
     ret %>% setDT 
     ret %>% setkeyv(by)
 
 # RENAME COLUMS  
 nms <- names(ret) %>% setdiff( ret %>% key)
 ret %>% setnames( nms, nms %>% name_bins(src=x, suffix=suffix) )

 return(ret)
 
}
