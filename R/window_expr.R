#' window_expr
#' 
#' generate an expression that can be \code{eval}'d on the event 
#' data set to keep only the window of relevant events to the features
#'
#' @param milestone character; name of milestone_dt column 
#' @param event_dt character; name of date column related to the event
#' @param direction character: before, after, of 
#' @param span length of time for the window 
#' 
#' Window := direction + span 
#' 
#' before: Milestone - event_dt <= window  (LEADING INDICATOR)
#' after : Milestone - event_dt >= window  (LAGGING INDICATOR)
#' within (of): abs(Milestone - event_dt ) <= windows     
#' 
#' @return 
#'   An unevalutated call object that can be evaluated in the milestone-event dataset.
#' 
#' @examples
#'   window_expr( "milestone_dt", "event_dt", ddays(2) )
#'   window_expr( "milestone_dt", "event_dt", ddays(3), "within" )
#'   window_expr( "milestone_dt", "event_dt", ddays(4), "before" )
#'   window_expr( "milestone_dt", "event_dt", ddays(5), "after" )
#'   
#' @export

window_expr <- function( milestone, event_dt, direction="of", span ) {
  
  span <- as.numeric(span)
  
  template <- switch( direction  
      , before = 'dp.misc::diffdays( {{milestone}}, {{event_dt}} ) <= {{span}} & dp.misc::diffdays( {{milestone}}, {{event_dt}} ) >= 0'
      , after  =  'dp.misc::diffdays( {{event_dt}}, {{milestone}} ) <= {{span}} & dp.misc::diffdays(  {{event_dt}}, {{milestone}} ) >= 0'
      , of = 'abs( dp.misc::diffdays( {{milestone}}, {{event_dt}} ) ) <= {{span}}' 
  )
  
  tdata <- list( 
      milestone = milestone
    , event_dt = event_dt    # name of column on the event data set
    , span = span
  )
  
  return( parse( text=whisker.render( template, tdata ) )[[1]] )
  
}