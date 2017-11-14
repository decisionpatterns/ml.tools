#' ggSeperate 
#' 
#' Separation plot for a numeric values and classes  
#' Separation
#' 
#' @param x object such as a vector
#' @param y object depends on x


ggSeparate <- function(x,y) UseMethod('ggSeparate')

ggSeparate.caret <- function(x,y) {}

# Example 
# dat_train.[ Number_of_Days_Delinquent <= 7, ] %>% 
#   ggplot( aes( x=response. ) ) + 
#   geom_histogram( aes(y=..count.., fill=response ) ) + 
#   # facet_grid( response ~ . ) +
#   scale_y_sqrt( label=comma, "Number of Accounts") +
#   scale_x_continuous( "DQ60+ Likelihood") + 
#   theme( legend.position = "none" )