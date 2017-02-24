#' Generate features at the correct grain.
#' 
#' bin-split-apply-combine-spread
#' 
#' @details 
#' 
#' This a decoration of the split-apply-combine pattern : 
#' bin-(split-apply-combine)-spread where multiple of these steps are linked
#' 
#' @examples 
#' 
#'  mpg %>% f_bin("cty")
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
#'  mpg %>% f_bin("trans", c('year','cyl'), bin=partial( fct_lump, n=6 ), summary=.cnt )
#'
#' @notes 
#'
#' The choice ot summary function is related to 
#' 
#' @export 

f_bin <- function( .data           # snapshot or fact data
                   , x             # string
                   , by=NULL       # ? getOption('response.grain') #' @param 
                   , bin = cut_n   #' binning function 
                   , ...           #' arguments passed to bin
                   , summary = .cnt #' summary function
                   , fill    = 0   #
                   , suffix  = NULL 
) { 
 
  # CREATE: .bin variable for splitting and summarizing x for bin identities
  ret <- 
    .data %>%
    
     # BIN 
     mutate( .bin = x %>% as.name %>% eval %>% bin(...) ) %>%
  
     # SPLIT-APPLY-COMBINE: Summarize at the correct grain 
     summary(x,by) %>% 
  
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

 
#' This is the split-apply-combine pattern 
#'
#' Recipe for frequency variables: 
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




#' @param .data data,table
#' @param x character variable name to be summarized
#' @param by 
#' 
#' @section feature names:
#' 
#' Examples: 
#' 
#' - var.[2/5].n
#' - var.[-Inf 10].n
#' - var.[-Inf 10).ρ
#' - var.[value].n
#' - var:value.n
#' 
#' @examples 
#' 
#' f_bin.cont( mpg, "displ", "year" )
#' mpg %>% f_bin.cont( x="displ", by=c("year","manufacturer" ) )
#' 
#' # DPLY
#' tbl <- 
#'   mpg %>% 
#'     mutate( .bin = quote(cty) %>% eval %>% cut_n(5) )  %>% 
#'  
#'  # tbl %>% 
#'  count_( c('year','.bin') ) %>%   # count
#'    mutate( value = n / sum(n) )  %>%   # add for freq
#'  
#'  # tbl <- tbl %>%       
#'  # group_by_( .dots=c('year','.x_bin') ) %>%  summarize( value=n() ) %>% 
#'  
#'  select_(.dots=c('year','.bin', 'value')) %>% 
#'   
#'  spread_( '.bin', value='value', fill=0 ) 

f_bin.cont <- function( .data, x, by=NULL , nbins=nclass.scott, summary=list(data.table::.N), ... ) { 

  if( ! exists(x, .data ) ) stop( paste0(x, " does not exist in the data.") )
  
  #' SPLIT: CUT LIST 
  #' A dply way:
  #' mpg %>% mutate( cty = quote(cty) %>% eval %>% cut_n(5) ) %>% select(cty)
  x_list = 
    .data[ , lapply(.SD, cut_n, nbins=nbins ), .SDcols=x  ] %>% as.list
  
  
  # BY LIST: MATCHES THE GRAIN OF THE ANALYSIS
  # Generally these are not touched but are instead passed through 
  by_list = .data[ , by, with=FALSE ] %>% as.list()
  
  # MORE GENERIC:
  # by_list = 
  #   by %>% 
  #   sapply( USE.NAME S=TRUE, simplify=FALSE,
  #     function(x) .data[[x]]                 # COULD CONTAIN ADDITIONAL       
  #   )
  
  # BUILD EITHER:
  # - [✓] by list name = values  
  # - [ ] DT for by-without-by .data[ , li]
  
  
  # dcast FORMULA:  ... ~ x_1 + x_2 + ... 
  
  form <- ... ~ rhs 
  rhs(form) <- x %>% collapse("+") %>% parse(text=.) %>% .[[1]]
  
  # APPLY 
  ret <- 
    cnt( .data, x_list, by_list  )  %>%            # summarize
    dcast( form, fill=0, value.var="value" )      # pivot
  
  
  # NAMES: Apply standardize naming conventions 
  nms <- names(ret) %>% setdiff( ret %>% key)
  ret %>% setnames( nms, name_bins( nms, src=x, suffix="n" ) )
  
  return(ret)
}




#' Create long form count summarization
#' 
#' Create by, var, 
#' 
#' @param .data data.table; fact or snapshot data
#' @param x list (ie name=value) list of expressions for target variable
#' @param by list; (ie name-value) .dlist of expressions for by variables that define the 
#' response grain.
#'
#' @details  
#' 
#' @return 
#' long format summarization as a data object with columns for \code{by}, 
#' \code{x} and \code{value}.
#' 
#' @references 
#'   \url{http://stackoverflow.com/questions/28616829/r-group-by-with-custom-functions}
#'
#' @examples 
#'
#' by = list()
#' x = list( displ )
#' mpg %>% cnt(x=.(class), by=.(trans) )
#'   
#' @export 

#' 
#' 
#' @examples 
#' 
#' mpg %>% freq( x=by_list(mpg,"cty"), by=by_list(mpg, "year" ) )
#' 
#' 
#' mpg %>% freq( "cty", "year" )
#' 
#' x %>% mpt[ x=x, by=by ]
#' x %>% dcast( ... ~ cty , value.var="freq", fill=0 )
#' mpg %>% freq( x=by_list(mpg,"cty") )

freq <- function(.data, x, by ) {

  if( ! is.list(x) )  x  <- by_list(.data,x)
  if( ! is.list(by) ) by <- by_list(.data,by)  
  
  by_combo <- append(by,x)
  small <- .data[ keyby=by_combo, , .N ]
  big   <- .data[ keyby=by, , .(total=.N) ]
  
  by
  small[ big, on="year", .(year, cty, freq=N/total), .SDcols=qw(year,cty) ]
  small[ big, on=by, .(year,trans,freq=x.N/total), drop=FALSE ]
  
}

#' Use dplyr to summarize by x
freq <- function( .data, x, by ) { 
  
  small <- .data %>% group_by_( .dots=c(x,by) ) %>% summarise( n=n() )
  big <- .data %>% group_by_( .dots=by ) %>% summarise( total=n() )
  
  small %>% inner_join(big, by=by) %>% 
    mutate( freq=n/total ) %>% 
    select_(.dots=c(x,by, 'freq')) %>% 
    spread_( x, value="freq", fill=0 ) 
  
}

#' .data %>% freq( "trans", "year" )


#' Turn a list of variables into a by list
#'
#' @return 
#'  
#' names list; values are the names 
#'  
#' @examples 
#' 
#' mpg %>% by_list( c("year","trans" ) )
#' 
#' @note belongs in data.table.plust
#' @export 

by_list <- function(.data, x) { 
  .data[ , x, with=FALSE  ] %>% as.list
}

#' Create a table for use in data.tables i slot
#' 
#' @param .data data.table 
#' @param x character; name of character columns

i_table <- function(.data, x) { 
  .data[, x, with=FALSE ]  
}


#' @examples  
#' 
#'   mpg %>% f_bin.cat( )
  
f_bin.cat <- function( .data, x, by=NULL, lump=identity ) { 

  if( ! exists(x, .data ) ) stop( paste0(x, " does not exist in the data.") )
  
  # CREATE `by_list``
  x_list = list( .data[[x]] %>%  lump ) %>% structure(names=x) 
  
  if( ! is.null(by) ) { 
    bys = sapply( by, function(x) .data[[x]], USE.NAMES=TRUE, simplify=FALSE )
    by_list <- append(bys, by_list)
  }

  # NEED TO USE FORMULA INTERFACE  
  form <- ... ~ rhs 
  rhs(form) <- as.name(x)
  
  # CALCULATE AND CAST
  ret <- .data[ by=by_list, , .N ] %>%   
    dcast(form, value.var="N", fill=0)
  
  # Categorical names are based on x and the categories, eg. var.[value].n 
  
  ret %>% setnames( names(ret) %>% setdiff( ret %>% key), names_bin_count(x,nbins) )
  
  return(ret)
}



# data.table 

## CONTINUOUS 
mpg[ by=cut( mpg$cty, breaks=breakpoints( mpg$cty, nbins=5 ) ) , , .N ] %>% 
  # dcast( . ~ cut, value.var="N", fill=0 )  # returns data.table
  spread_( "cut", "N", fill=0)               # returns data.frame
  
  
mpg[ by=.( year=mpg$year, cut( mpg$cty, breaks=breakpoints( mpg$cty, nbins=5 ) ) ), , .N ] %>% 
  # dcast( year ~ cut, value.var="N", fill=0 )
  spread_( "cut", "N", fill=0)  
  
## CATEGORICAL 
mpg[ by=trans, , .N ] %>% 
  dcast( . ~ trans, value.var="N")


# DPLYR

## CATEGORICAL
mpg %>% group_by_("year","trans") %>% count_("trans") %>% 
  spread_( "trans", "n", fill=0  ) 

ex <- expression(substr(manufacturer,1,3))

## CONTINUOUS 
#' This is dependent upon creating a dynamic by group.

mpg %>% group_by( x=substr(manufacturer,1,3) ) %>% count # summarise( n=length(manufacturer) )

mpg %>% group_by( x=eval(expression(manufacturer)) ) %>% count

mpg %>% 
  group_by( x=eval(as.name('manufacturer')) ) %>% count



ex <- expression( substr(manufacturer,1,3) )
mpg %>% group_by_( x=ex ) %>% count
