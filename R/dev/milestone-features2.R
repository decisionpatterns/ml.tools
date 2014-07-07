Milestones[ , cutoff_dt := max(GA_Parent_Daily$visit_dt)]

# Add constant milestones 
Milestones[ , xmas_2013   := ymd(20131225) ]
Milestones[ , easter_2014 := ymd(20140420) ]

  # Lifecycle_Events <- Milestones 
  
  ga_summable <- cqq( 
      organic_searches, entrances, visits, new_visits, pageviews
    , unique_pageviews, visit_duration, avg_visit_duration, quantity
    , total_value, transactions, unique_purchases, average_order_value
    , revenue, total_abandonment_rate, bounces 
  ) 
  events = measures = GA_Parent_Daily[ , c( 'parent_id', 'visit_dt', ga_summable ), with=FALSE ]
  setkeyv( measures, key(GA_Parent_Daily)) 
  directions = cqq( before, after, of )
  spans = c( 1, 2, 5, 10, 15, 30, 60)   # days

  event.li = divide( ev_ga, by='parent_id' )

template <- "{{feature_prefix}}.{{measure_nm}}.within_{{span_text}}_{{time_grain}}_of.{{milestone_nm}}"

#' feature_name
#' 
#' build a feature name from variables on the environment
#' @param data where to evaluate the template, default=parent.frame() 
#' 
#' @export
feature_name <- function( data=parent.frame(), ... ) {
  template <- "{{feature_prefix}}.{{measure_nm}}.within_{{span}}_{{time_grain}}_{{direction}}.{{milestone_nm}}"
  whisker.render(template, data=data, ... )
}
feature_name()

time_grain = "days"

# comb = data.table:::merge.data.table
comb = cbind  
# comb = base::c

# TODO: NERGE CAN MOVE HIGHER IN THE 
  
  
x <- 
  foreach( milestone = ifeatures( Milestones ), .combine = comb, .multicombine=TRUE, .packages=c("whisker","dp.misc") ) %:%  # milestones / milemarkers
system.time(  
x <-   foreach( measure = ifeatures( measures ), .combine = comb, .multicombine=TRUE  ) %:%   # events
  foreach( direction = directions, .combine = comb, .multicombine=TRUE  ) %:%            # characer: before, after, within
  foreach( span = spans, .combine = comb, .multicombine=TRUE
               , .packages=c("whisker","dp.misc"), .export=cqq(parents, feature_prefix, time_grain) ) %dopar%                   # durations: 
    {
      milestone_nm <- setdiff( names(milestone), key(milestone) )
      measure_nm   <- setdiff( names(measure), key(measure) )
      event_dt     <- "visit_dt"  # index column
      
      span_text    <- span   # USED IN FEATURE NAME NOT WINDOW EXPR
      
      # 
      expr <- window_expr( milestone_nm, event_dt, direction, span )
      
      # time_grain = time_grain 
      # feature_prefix = feature_prefix # Move into this frame 
      
#       x. <- inner_join( milestone, measure) %>%   #
#              filter( eval(expr) ) %>%
#              select( c("parent_id" ) ) 
      
      x <- merge( milestone, measure, by="parent_id" ) # , all.x=TRUE, allow.cartesian=TRUE )

      x <- x[ eval(expr), by=parent_id
          , sum(.SD)
        # , .SD[ , measure_nm, with=FALSE ]
        # , lapply( .SD[ , measure_nm, with=FALSE ], sum, na.rm=TRUE ) 
        # , lapply( .SD[ , measure_nm, with=FALSE ], sum, na.rm=TRUE, .SDcols=measure_nm  )  
        , .SDcols=measure_nm
      ]
  
      setnames( x, 'V1', feature_name() )
      # setnames( x, measure_nm, feature_name() )
      x <- merge( parents, x, all.x=TRUE )
      x <- na.replace( x )
      x[ , parent_id := NULL ] 
      return(x)
      
    }
)
# foreach( i = 1:3, .combine=unlist ) %:% foreach(j=1:5) %do% { i*j } 
# foreach( i=isplitCols(iris, chunkSize=1), .combine=cbind, .inorder=TRUE ) %:% foreach( j=isplitCols(mtcars, chunks=2), .combine=cbind) %do% { i }