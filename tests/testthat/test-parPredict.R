library(testthat)
library(ml.tools)
library(doParallel)
  registerDoParallel(4)

#' require(randomForest) 
data(iris)

# Linear Model
  fit  <- lm(Sepal.Length  ~ ., iris)
   
  p  <- predict( fit, iris )
  pp <- predict.parallel( fit, iris )
    
  expect_true( 
    all(p== pp)
    , info="predict.pararallel is not equivalent to predict for lm" 
  )

# message randomForest
  require(randomForest)
  
  fit  <- randomForest( Sepal.Length  ~ ., iris )
  
  p  <- predict( fit, iris )
  pp <- predict.parallel( fit, iris )
    
  expect_true( 
    all(p== pp)
    , info="predict.pararallel is not equivalent to predict for randomForest" 
  )
