#' Histogram Feature
#' 
#' A **histogram feature** turns a continuous or categorical distributions of 
#' values into features that can be used to help estimate/predict a that 
#' provides contains distributional information. Consider the case where you 
#' want to forecast the revenues of a
#' company and those revenues are related to the `type` of customer. One set of 
#' features is the count of each of each `type`. ^[If the cardinality of `type` 
#' is `p` than there will be `p` features created.]   
#' 
#' Similarly, there might be a numeric feature that has repeated measurements 
#' per period (or in aggregate). The revenue forecast may be related to the 
#' 
#' 
#' ## Histogram types 
#' 
#' There are 2 types of histogram feautes, Count and Position:
#' 
#' ### Counts Features 
#' 
#' - counts measure the number of each observation falling in each bin
#' - frequency is related to counts, but normalizes all bins so that the sum of
#'   the values will equal 1 in each bin 
#'  
#' Count features apply to both continuous and categorical data and are easy to 
#' intpret. Count features may be more appropriate when you are trying estimate
#' an summable function where the total magnitude of the counts relate to the 
#' response. Frequency estimates are likely more appropriate for estimating 
#' categories and non-summable responses.
#'   
#' ### Postion Features
#'  
#' Position measures report the placement of bins, the apply to continuous 
#' values only.
#' - mids measure the midpoint of each bin. 
#' - breaks measure the position of the midpoint of e each bin, applies to continuous 
#' values only
#' 
#' It can be used 
#' 1. Create a numeric measure for each group 
#' 
#' There are several types of histogram featuresS
#' 
#' Histogram features should be deterministic with relations between training 
#' and scoring data. 
#' 
#' Most data is heavily skewed and so the histogram feature should account for it 
#' The Goal is to produce a feature aggregates to the level of the key. There 
#' are different types of 
#' 
#' ## Process 
#' 
#' 
#' 
#' There two grains involved.
#' - Snapshot
#' - Forecast (period)
#' 


# var = "SponsorDimAccountID"
# var = as.name('SponsorDimAccountID')
# # x <- 
# #   c_snapshot[ by=Period, , .(x = eval(var) %>% n_distinct )  ]
# k <- qw( Period, SponsorDimAccountID )
# 
# c_snapshot[ keyby=k, , .N] %>% key
# 
# x <- c_snapshot[ by=list(Period, SponsorDimAccountID), , .N]
# x2 <- x[ by=Period, , bin_count_list(N) ]
#' x2 %>% setnames( x2, )
# 
# x2  %>% as.xts()  %>% autoplot 





#' Binned Features utilities for data.tables aggregations
#' 
#' This Binned Features are created in order to use data when it has to be 
#' aggregated to the grain of the response (y). This happens whenever the 
#' goal is to predict an aggregate value in which there multiple observations 
#' used in estimating each y value.[^x is said to be more *granular* or at a 
#' *higher dimension* than y.]  A prerequistite of ML algorithms is that there
#' is a one-to-one relationship between the predictors(x) and responses(y). When
#' there are multiple observations for each y, the mismatch in dimensionality 
#' must be reconciled.
#' 
#' ## X Dimensionality Reduction
#' 
#' One resolution is to reduce the dimensionality of x. The multiple observations
#' of x's form a distribution. Commonly each x distribution is summarized by a
#' measure of central tendancy, usually the mean, median or mode of the x 
#' distribution.  While often adequate, this can sometimes miss nuances and 
#' important features of the data.  
#' 
#' ## Y Dimensionality Expansion
#' 
#' Another approach is to expand the dimensionality of x. 
#' 

#' 
#' Collapsing dimensions 
#' yield multiple x values for each y value.  In this case there will be 
#' multiple x values for each 
#' by collapsing one or more dimension from fact/detail data and aggregating at 
#' the grain of the response (variable). 
#'  
#' binning a distribution of values or items into 
#' bin_quantile_breaks_list give the break points for a quantile
#'  
#' @param x numeric vector
#' @param ... additional arguments passed to \code{\link[stats]{quantile}}
#' 
#' @details 
#' 
#' In each case these function produce a list of values with descriptive names. 
#' Parameters control the number of bins created.
#' 
#' \code{bin_quantile_breaks_list} gives the quantile break points for a 
#' continuously. 
#' 
#' @return 
#'  a list with quantiles of the period. Quantile names are prefixed with 'q' 
#'  for quantile.
#'  
#' @seealso 
#'   \code{\link[stats]{quantile}} which is used to implement 
#' 
#' @examples 
#' 
#'   bin_quantile_breaks_list( rnorm(1000) )
#'   x[ by=Period, , bin_quantile_breaks_list(N) ]
#' 
#' @export


bin_quantile_breaks_list <- function(x, ...) { 
  
  li <- as.list( quantile(x, ...) )
  names(li) <- paste0("q", names(li) %>% gsub( "\\%$", "", . ) )
  
  return(li)
  
}


#' @details 
#' 
#' \code{bin_count_list} divides the range of \code{x} into equal-width buckets the 
#' lowest and highest buckets are then expanded to include -Inf and +Inf 
#' respectively.
#' 
#' @seealso 
#'   \code{dp.misc::segment}
#' 
#' @examples 
#' 
#'   x[ by=Period, , bin_count_list(N) ]

bin_count_list <- function(x, breaks=5, ...) { 

  # Convert breaks to vector
  if( length(breaks)==1 ) { 
    br <- min(x) + (0:breaks) * diff(range(x))/breaks
    br[1] <- -Inf
    br[ length(br) ] <- +Inf
  } else {
    br <- breaks
  }
  
  cnts <- hist(x, breaks=br, plot=FALSE)$counts 
  
  li <- as.list(cnts)
  names(li) <- paste0("bin_", 1:length(li), "of", length(li) )
  return(li)
  
}

#' @details 
#' 
#' \code{bin_freq_list} gives the frequencies in the histogram. Frequencies are
#' counts normalized to 1.  It uses \code{bin_count_list}
#' 
#' @examples
#' 
#' rnorm(100) %>% bin_freq_list

bin_freq_list <- function(x, breaks=5, ...) {

  li <- bin_count_list(x, breaks=breaks, ...)
  tot <- li %>% unlist %>% sum
  li <- lapply(li, function(x) x / tot ) 
  
  return(li)

}

#' @details 
#' 
#' \code{bin_density_list} reports \code{hist}'s density values. 
#'
#' @examples 
#' 
#'   rnorm(1000) %>% bin_density_list
#'   x[ by=Period, , bin_density_list(N) ]

bin_density_list <- fbin_count_list <- function(x, breaks=5, ...) { 
  
  # Convert breaks to vector
  if( length(breaks)==1 ) { 
    br <- min(x) + (0:breaks) * diff(range(x))/breaks
    br[1] <- -Inf
    br[ length(br) ] <- +Inf
  } else {
    br <- breaks
  }
  
  cnts <- hist(x, breaks=br, plot=FALSE)$density 
  li <- as.list(cnts)
  names(li) <- paste0("density_", 1:length(li), "of", length(li) )
  return(li)
  
}