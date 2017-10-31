#' Mutlinomial glm 
#' 
#' Fits a multinomial logistic regression using glm models.
#' 
#' @import formula.tools
#' 
#' @examples 
#' 
#'  data(iris)
#'  glm_multinomial( Species ~ . , data)
#'  
#'    
#' @export 

glm_multinom <- function(formula, data, ...) {
  
  
  lh <- lhs.vars(formula, data=data )
    
  # Convert to factor (if not one already)
  data[[lh]] <- factor( data[[lh]] )   

  fits <- list()  # container for models
  lvls <- levels( data[[lh]] )
  for( lvl in lvls ) {
    
    not_lvl <- paste0("!",lvl) 
    data2 <- data
  
    data2[[lh]] %<>% equals(lvl)
    
    fit <- glm( form, data=data2, family=binomial ) 
    
    # fit.li <- structure( list(fit), .Names=lvl )
    # fits <- c(fits, fit.li)
    # fit <- structure( list(fit), .Names = lvl ) 
    fits[[ length(fits)+1 ]] <- fit
  }  
  names(fits) <- lvls
  
  ret <- list()
  
  # FITTED: PROBS 
  fv <- lapply( fits, function(x) x$fitted.values ) %>% as.data.frame
  ret$fitted.values <- fv
  ret$fitted.probs <- fv
  
  # FITTED: CLASS
  which_col <- fv %>% apply(1,which.max)
  ret$fitted.class <- names(fv)[ which_col  ]  
  
  # FITTED: PROBS (LINK)
  ret$fitted.probs <- logic
  
  fv.link <- logistic_inv( fv )

  
  # FITTED: NORMALIZED  
  fv.norm <- fv %>% apply(1,pct ) %>% t
  # fv.softmax <- fitted %>% apply(1,softmax) %>% t
  
  probs <- ( 1:nrow(fv.norm) ) %>% sapply( function(i) fv.norm[i, which_col[i] ] )
  
  # RESIDUALS
  fv.residuals <- 1-probs
  
  
  # COEFFICIENTS 
  coefs <- sapply(fits, coef)
  
  
  ret  
    
}




#' Predict multinomial GLM model
#' 
#' Predicts a multinomial GLM model
#' 
#' @param x glm_multinomial object
#' @param newdata, data.frame
#' @param type string; one of 'norm' (default), 'probs', 'raw' or 'class'
#' @param ... unused
#' 
#' @details 
#' 
#' @return 
#' 
#'  - For `type="raw"`  : A matrix with nrow(newdata) rows and one prediction per model
#'  - For `type="probs"`: A matrix of raw probabilities
#'  - For  type="norm"  : A matrix of normalized probabilities 
#'  - For  type="class" : A vector of predicted classes
#'    
#' @export

predict.glm_multinomial <- function(x, newdata, type=c("norm", "probs", "raw", "class"), ... ) { 

  type <- match.arg( c("norm", "probs", "raw", "class"))
  coefs <- x$coefs
  vars <- rownames(coefs)
  
  intercept_lbl = "(Intercept)"
  
  newdata <- newdata[ , vars %>% setdiff(intercept_lbl) ]
  
  # Add intercept ?
  if( vars[[1]] == intercept_lbl ) 
    newdata <- data.frame( intercept = 1, newdata[ ] )  
  
  # SCORE
  newdata <- as.matrix(newdata)
  ret <- newdata %*% coefs
 
  
  # TYPE: RAW
  if( type=="raw") return (ret)
  
  # TYPE: CLASS
  if( type=="class" ) { 
    which_col <- raw %>% apply(1,which.max)
    class <- colnames(raw)[ which_col  ]  
    return( class )
  }
  
  # TYPE: PROBS 
  ret <- logistic(ret)
  if( type=="response") return( ret )
  
  
  # PREDICT: NORMALIZE PROBABILITES 
  if( type=="norm" ) {
    ret <- ret %>% apply(1, pct ) %>% t
    ret
  }
  

}
