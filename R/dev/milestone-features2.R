milestone_features <- function(...) {

Milestones[ , cutoff_dt := max(GA_Parent_Daily$visit_dt) ]
Milestones[ , cutoff_dt := q_cutoff ]

# Add constant milestones 

  # Lifecycle_Events <- Milestones 
  
  ga_summable <- cqq( 
      organic_searches, entrances, visits, new_visits, pageviews
    , unique_pageviews, visit_duration, quantity
    , total_value, transactions, unique_purchases, average_order_value
    , revenue, bounces 
  ) 

  # ga_averages <- c( avg_visit_duration, average_order_value, total_abandonment_rate )

  events = measures = GA_Parent_Daily[ , c( 'parent_id', 'visit_dt', ga_summable ), with=FALSE ]
  setkeyv( measures, key(GA_Parent_Daily)) 

  directions = cqq( before, after, of )
  spans = c( 1, 2, 5, 10, 15, 30, 60 )   # days
  feature_prefix = "ga"

  time_grain = "days"

  comb = data.table:::merge.data.table
  # comb = cbind  
  # comb = base::c

  # event.li = divide( ev_ga, by='parent_id' )

# template <- "{{feature_prefix}}.{{measure_nm}}.within_{{span}}_{{time_grain}}_of.{{milestone_nm}}"

#' feature_name
#' 
#' build a feature name from variables on the environment, e.g:
#'   ga.visits.withing_2_days_before.xmas2013
#' @param data where to evaluate the template, default=parent.frame() 
#' 
#' @export
feature_name <- function( data=parent.frame(), ... ) {
  template <- "{{feature_prefix}}.{{measure_nm}}.within_{{span}}_{{time_grain}}_{{direction}}.{{milestone_nm}}"
  whisker.render(template, data=data, ... )
}

# feature_name()


# TODO: NERGE CAN MOVE HIGHER IN THE 

try( rm(x) )
  
x <- 
  foreach( milestone = ifeatures( Milestones ), .combine = comb, .init=parents ) %:%  # milestones / milemarkers
x <-  foreach( measure = ifeatures( measures ), .combine = comb  ) %:%   # events
  foreach( direction = directions, .combine = comb  ) %:%            # characer: before, after, within
  foreach( span = spans, .combine = comb # , .multicombine=TRUE
        , .packages=c('whisker','ML.tools','dp.misc','na.actions'), .export=cqq(feature_prefix, time_grain) ) %dopar%                   # durations: 
    {

      milestone_nm <- setdiff( names(milestone), key(milestone) )
      measure_nm   <- setdiff( names(measure), key(measure) )
      event_dt     <- "visit_dt"  # index column
      
      x0 <- merge( milestone, measure, by="parent_id" ) # , all.x=TRUE, allow.cartesian=TRUE )
      
      expr <- window_expr( milestone_nm, event_dt, direction, span ) 
      x <- x0[ eval(expr), ]
      
      if( nrow(x)==0 )
        x <- x0[ , list(parent_id = parent_id, V1=0 )]
      
      if( nrow(x)>0 ) 
        jexp <- parse( text=paste( "sum(", measure_nm, ")" ) )[[1]]
        x <- x[ by=parent_id,
            , eval(jexp)
        ]
      
        #   x <- x[ by=parent_id,
        #       , sum(.SD[[ measure_nm ]])
        #       , .SDcols=measure_nm
        #   ]

      setnames( x, 'V1', feature_name() )
      
      x <- merge( parents, x, all.x=TRUE )
      x <- na.replace( x )
      # x[ , parent_id := NULL ] 
      return(x)
      
    }

)


}

# pi = 42599025
