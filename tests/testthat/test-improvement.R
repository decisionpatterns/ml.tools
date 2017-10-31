
context( "improvement")

test_that( "improvement", {
  
  metric <- 1.1 
  ref    <- 1.0 
   
  improvement( metric, ref ) %>% expect_equal(0.1)
  improvement( metric, ref, decreasing = TRUE ) %>% expect_equal(-0.1)
  
})
