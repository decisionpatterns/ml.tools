

feature_name.template <- "{{feature_prefix}}.{{measure_name}}.within_{{span}}_{{time_grain}}_of.{{milestone_name}}"
feature_set_name.template <- "{{feature_prefix}}.within_{{span}}_{{time_grain}}_of.{{milestone_name}}"

feature_name <- function( feature_name ) { 
  

}

# EVENT FEATURES
 
  # Define various time windows
  # since these are day window(s) 
  spans = c( ddays(1), ddays(2), ddays(7), ddays(14), ddays(28) )
  # window_names = paste0( as.numeric(windows/ddays(1)), "_days" )   
  
  # milestones <- setdiff( names( Milestones), "parent_id" ) 
  ga_summable <- cqq( 
      organic_searches, entrances, visits, new_visits, pageviews
    , unique_pageviews, visit_duration, avg_visit_duration, quantity
    , total_value, transactions, unique_purchases, average_order_value
    , revenue, total_abandonment_rate, bounces 
  ) 

  ev_ga  <- Milestones[ GA_Parent_Daily ]
  events <- ev_ga[1:5000]

  event <- 'visit_dt'  # This could probably be stored in metadata
  feature_prefix <- "x_ga"


#' Make Milestone Features
#'
#' Create a list of feature data.tables 
#' 
#' @param milestones data.table at a keyed column defining the subject as well 
#' as and one or more date(POSIXct) columns indicating the milestones for each 
#' subject    
#' @param events data.table with events for each subject; it should have a key
#' that matches the keys of \code{milestone}  
#' @param windows Duration; vector of durations to evaluate 
#' (before|after|within) milestone
#' @param direction
#' 
#' Concept:
#'
#'   Feature := 
#'      [data source]
#'      .measure
#'      .[window] := direction + span
#'      .[aggregate function] (default:sum)
#'      .[reference]
#'      
#'  Operation Order := Milestone (data.table) > Windows ...  >  aggregation
#'      
#' 
#' @return 
#'   a list of feature data.frames each with a subject key and appropriately 
#'   named features 
#'   
#'@note
#' * HOW DO WE PASS IN ADDITIONAL ARGUMENTS(?) TO FUNCTION
#' * HOW DO WE GENERATE FEATURES AT THE HIGHER LEVEL(?) COUNT OF ...
#' 
#' TODO: milestones should be denoted as dates
#' 
       
  #`divide` Milestones into list of data.frames: parent_id => milestone date
  # Milestones are defined in the preparation faces of munging 
  Milestones.li <- divide( Milestones, "parent_id")
  
  feature_list <- list() 
  # For each resulting milestone compute a feature.table
  for( milestone_name in names(Milestones.li) ) {
    MS <- Milestones.li[[milestone_name]]      # 
    MS <- MS[ GA_Parent_Daily ]   # merge with data
    
    
    
    # LOOP OF WINDOWS
    
   
    window      = windows
    window_name = window_names
    
    for( span in spans ) {
      
      span = span / ddays(1)
  
      directions = cqq( before, after, within )  # THIS WILL LATER BE AN ATRIBUTE OF THE DATA
      for( directions in directions ) {
        
        # Apply the date range ... expensive 143,53 seconds
        # window_name.: name for feature window, e.g. 'within_2_days_before'
        window_name. <- paste0( "within_", window_name, "_",  direction )
        exp <- window_expr( milestone_name, event, span=ddays(2), direction=direction ) 
  
        # FEATURE SET NAME
        feature_set_name <- whisker.render( feature_set_name.template )
        
        # NAME FEATURES:     
        #    prefix.[measure_name].
        measure_names <-  setdiff( names(x), "parent_id" )
        feat_names <- paste( feature_prefix, measure_names, window_name., milestone_name, sep="."  )
         
        #' feature_name.template
        feature_names <- whisker.mapply( 
          feature_name.template, measure_name=measure_names, span="2", time_grain="days" 
        )
        
        milestone_expr( )
        
        system.time( 
          x <- events[ eval(exp), by=parent_id, lapply( .SD[ , ga_summable, with=FALSE ], sum, na.rm=TRUE  )  ]  # sum features since date
        )                                                   # ^^^^^^^^^^
        
        # CREATE NAMES
  
        setnames( x, measure_names, features_names )
        
        feature_list[[ featset_name ]] <- x
      
      } # END directions
    
  } # END milestones