
context( "imp[rovement]")

test_that( "imp", {
  
  metric <- 1.1 
  ref    <- 1.0 
   
  imp( metric, ref ) %>% expect_equal(0.1)
  imp( metric, ref, decreasing = TRUE ) %>% expect_equal(-0.1)
  
})
