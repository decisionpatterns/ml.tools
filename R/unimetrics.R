#' unimetrics / allmetrics
#' 
#' Measure variable performance singular or as
#' 
#' @param metric string; See [train](caret::train())
#' 
#' @examples 
#' 
#' data(iris)
#' 
#' 
#' 
#' 
#' @import catcont caret formula.tools

unimetrics <- function( data
                  , formula
                  , metric = ifelse( is_cat( data[[lhs]] ), "Accuracy", "RMSE" ) 
                  , family = ifelse( is_cat( data[[lhs]] ), binomial, gaussian ) 
                  ) { 

  lhs <- lhs.vars(formula, data=data) 
  rhs <- rhs.vars(formula, data=data)
  
  
  
  form <- . ~ . 
  lhs(form) <- as.name(lhs)
  
  for( rh in rhs ) { 
  
    rhs(form) <- as.name(rh)
    
    fit <- glm( form, data, family=binomial )
    
    err_01 <- fit$y - round(fit$fitted.values)
    acc <- 1-sum(abs(err_01))/length(err_01)
  
    df <- data.frame( var=rh, acc=acc )
    if( ! exists('ret') ) 
      ret <- df else 
      ret <- rbind( ret, df )  
      
  }
  
  ret
}


# logistic_inv <- function (y) 
# -log(1/y - 1)




