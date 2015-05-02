#' Date Extras
#
#' Additional functions
#'
#' @param dates dates
#' @param ... passed to events::major_dates
#'
#' \code{days_to_holiday} calculates the days to the nearest event. \cr
#' \code{days_from_holiday} calculates the days from the most recent event \cr
#'
#' @examples
#'   days_to_holiday( today() )
#'
#' @export
#' @rdname date-extras

days_to_holiday <- function( dates, ... ) {

  data(events)

  # dates <- as.POSIXct( years )
  # if( ! is.POSIXct(dates) ) dates <- as.POSIXct(dates)

  years <- unique( lubridate::year(dates) )
  years <- c( min(years)-1, years, years+1 )

  hdays <- major_dates( years, ... )
    # if( is.null(tags) )
    #  major_dates( years, ... ) else major_dates( years, tags = tags )


  m <- outer( dates, hdays, FUN="difftime", units="days" )
  m <- -m
  dys <- apply( m, 1, function(x) min( x[ x>=0 ] ) )

  return(dys)

}

#' @examples
#'   days_from_holiday( today() )
#'
#' @export
#' @rdname date-extras
days_from_holiday <- function( dates, ... ) {

  dates <- as.POSIXct(dates)
  yrs <- unique( year(dates) )
  yrs <- ( min(yrs,na.rm=TRUE)-1 ):(max(yrs, na.rm=TRUE)+1 )

  hdays <- major_dates( yrs, ... )

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
#' @param dates date vector
#' @param ... additional arguments
#' @param label how to label the resulting features
#'
#' Calculates various attributes for a given date.
#'
#' @return a data.table with the following attributes: year, month, day,
#' week (week of the year), wday (day of the week), holiday (logical-
#' whether the day is a holiday, days_to_holiday, days_from_holiday
#'
#' @examples
#'   featurize_date( now() )
#'
#' @export
#' @rdname dates-extras
# @import lubridate

featurize_date  <- function(
  dates, label=NULL, ...
) {

  feats <- data.frame(
      date  = dates
    , year  = year(dates)
    , month = month(dates)
    , day   = lubridate::day(dates)
    , week  = week(dates)
    # , we_days = 8 - wday(dates)  # Sunday = 1
    # , we_days = ( ( 8 - wday(dates) )  %% 7 ) + 1
    , wday    = lubridate::wday(dates, label=TRUE )
    , we_days = days_to_we_end( dates )
    , is_holiday = isHoliday( dates, ... )
    , days_to_holiday = days_to_holiday( dates, ... )
    , days_from_holiday = days_from_holiday( dates, ... )
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
#' @param ... additional arguments passed to \code{isHoliday} such as \code{tags}
#'
#' @return the length of time in days between the date and the end of the
#' weekend, including long weekends
#'
#' @examples
#'  days_to_we_end( now() )
#'  days_to_we_end( ymd(20140831) )
#' @export

days_to_we_end <- function( dates, use.holiday=TRUE, ... ) {

  we_days = (( 8 - wday(dates) ) %% 7 ) + 1

  if( use.holiday ) {
    monday  <- ceiling_date( dates, "week" ) + ddays(1)
    tuesday <- ceiling_date( dates, "week" ) + ddays(2)

    we_days <- ifelse( isHoliday(monday, ...) , we_days + 1 , we_days )
    we_days <- ifelse( isHoliday(monday, ...) & isHoliday(tuesday, ...), we_days + 1, we_days )
  }

  return(we_days)

}

#' isHoliday
#'
#' @param dates POXSIXct vector to test
#' @param tags character; one or more tags indication that
#'
#' \code{isHoliday} determines whether dates fall on a holiday
#'
#' @return logical of the same length as \code{dates} indicating whether the date
#'   follows on a holiday
#'
#' @seealso
#'   \code{\link[TimeWarp]{isHoliday}}
#'
#' @examples
#'   isHoliday( ymd( 20140901, 20131225 ) )
#'   isHoliday( now() )
#'   isHoliday( ymd('20140901') )
#'
#' @export

isHoliday <- function( dates, tags=NULL ) {

  data(events)

  if( ! is.POSIXt(dates) ) stop( "dates is not a POSIXct vector" )

  dates <- round_date( dates, 'day' )

  years <- unique( lubridate::year(dates) )
  years <- sort( unique( c( min(years)-1, years, years+1 )  ) )

  hdays <-
  if( is.null(tags) )
    major_dates( years ) else major_dates( years, tags = tags )

  dates %in% hdays

}
