#' test-apply.pattern

context('apply.pattern')
  
  pattern <- data.frame( 
     letters=as.factor(letters[1:3])
   , LETTERS=as.factor(LETTERS[1:3]) 
   , numbers=1:3L
  )  
  
  data <- data.frame( 
     letters=as.factor(letters[1:4])
   , LETTERS=as.factor(LETTERS[1:4])
   , numbers=1:4L 
  ) 
       


  out <- apply.pattern( data, pattern )       
  expect_equivalent( out$letters[4], pattern$letters[1] )
  expect_equivalent( out$LETTERS[4], pattern$LETTERS[1] )
  expect_equivalent( out$numbers[4], 4)

  
  out <- apply.pattern( data, pattern, 2 )
  expect_equivalent( out$letters[4], pattern$letters[2] )
  expect_equivalent( out$LETTERS[4], pattern$LETTERS[2] )
  expect_equivalent( out$numbers[4], 4)

  out <- apply.pattern( data, pattern, length )
  expect_equivalent( out$letters[4], pattern$letters[3] )
  expect_equivalent( out$LETTERS[4], pattern$LETTERS[3] )
  expect_equivalent( out$numbers[4], 4)