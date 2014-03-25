#' Date Extras
# 
#' Additional functions 
#'
#' @param dates
#' @param type  (passed to TimeWarp::holidays)
#' @export
#' @rdname date-extras
days_to_holiday <- function( dates, type="NYSE" ) {
  
  yrs <- unique( year(dates) )
  yrs <- c( min(yrs)-1, yrs, yrs+1 )
 
  hdays <- holidays( yrs, type=type, silent=TRUE )
  m <- outer( dates, hdays, FUN="difftime", units="days" )
  m <- -m
  dys <- apply( m, 1, function(x) min( x[ x>=0 ] ) )
  
  return(dys)
  
}

#' @export
#' @rdname date-extras
days_from_holiday <- function( dates, type="NYSE" ) {
  
  yrs <- unique( year(dates) )
  yrs <- ( min(yrs,na.rm=TRUE)-1 ):(max(yrs, na.rm=TRUE)+1 ) 
 
  hdays <- holidays( yrs, type, silent=TRUE )

  if( min(dates, na.rm=TRUE) < min(hdays, na.rm=TRUE) ) 
    warning( "dates exist before earliest recorded holiday" )

  m <- outer( dates, hdays, FUN="difftime", units="days" )
  dys <- apply( m, 1, function(x) min( x[ x>=0 ] ) )
  
  return(dys)
  
}


#' Create features from a date
#' 
#' Create categorical date features 
#'
#' @param label how to label the resulting features
#'  
#' Calculates various attributes for a given date.  
#'
#' @return a data.table with the following attributes: year, month, day,
#' week (week of the year), wday (day of the week), holiday (logical-
#' whether the day is a holiday, days_to_holiday, days_from_holiday
#'
#' @export
#' @rdname dates-extras
featurize_date  <- function( dates, type="NYSE", label=NULL ) {
  
  feats <- data.frame( 
      date  = dates 
    , year  = year(dates)
    , month = month(dates)
    , day   = day(dates)
    , week  = week(dates) 
    # , we_days = 8 - wday(dates)  # Sunday = 1
    # , we_days = ( ( 8 - wday(dates) )  %% 7 ) + 1 
    , wday    = lubridate::wday(dates, label=TRUE )
    , we_days = days_to_we_end( dates ) 
    , is_holiday = isHoliday( dates, type=type)
    , days_to_holiday = days_to_holiday( dates, type=type )
    , days_from_holiday = days_from_holiday( dates, type=type )
    # , days_to_sunday = ceiling_date(dates, "week") - dates 
  )
  
  feats <- data.table( feats ) 
  # setkey( feats, "date" )
  
  if( ! is.null(label) )
    setnames( feats, "date", label )
   
  return(feats)

}


#' Count days to the weekend's end
#'
#' Count the number of days from the date to the end of 
#' the week, optionally accounting for Monday holidays
#' 
#' Considers Monday and Tuesday holidays, but will not consider
#' Wednesday ...
#'
#' @param dates Date objects
#' @param use.holiday Whether to add holidays are the end of
#' the weekend.
#' @param type Type of holiday, default: NYSE
#'
#' @export
days_to_we_end <- function(dates, use.holiday=TRUE, type="NYSE" ) { 

  we_days = (( 8 - wday(dates) ) %% 7 ) + 1

  if( use.holiday ) {
    monday  <- ceiling_date( dates, "week" ) + ddays(1) 
    tuesday <- ceiling_date( dates, "week" ) + ddays(2) 

    we_days <- ifelse( isHoliday(monday, type=type) , we_days + 1 , we_days )   
    we_days <- ifelse( isHoliday(monday, type=type) & isHoliday(tuesday, type=type), we_days + 1, we_days ) 
  }

  return(we_days)

}
