#' getFeatures (DEPRECATED?)
#'
#' Possibly deprecated function for evaluating a list of \code{features} from
#' \code{data}
#' 
#' @param data
#' @param by
#' @param features 
#' 
#' @export

getFeatures <-function (data, by, features) 
{     

  # COERCE TO EXPRESSION
    expr = substitute( as.list(features) )

  # FUNCTION TO BE APPLIED APPLY TO INDIVIDUAL DATA.FRAMES 
  #   GENERATES FEATURE(S) AS ONE LIST PER DATA.FRAME
    listFeatures<-function(x,expr) eval( expr, x )

  # RETURN: 
    as.data.frame(    
  
     lapply(  
  
       as.data.frame( 
         t(
            sapply( 
              split( data, by  ) , 
              listFeatures ,
              expr  
            )
         )
       )
  
       ,
       unlist
     )
  
   ) 

}  # END FUNCTION getFeatures

# TEST:
# getFeatures( test, test$acct, list( len=length(acct), max=max(amt) )  )
# getFeatures( test, test$acct, list( len=length(acct), max=max(amt), min=min(amt) ) )

# TODO:
# - make 'by' look in data if it doesn't exist on the current frame
# - trap for one feature
