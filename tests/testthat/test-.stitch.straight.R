library(testthat)
library(ml.tools)
library(data.table)
# require(dp.misc, quietly=TRUE)  # TEMPORARY for .dup action

data(mtcars)
cars <- mtcars
cars$model <- rownames(mtcars)
setDT(cars)
setkey( cars, model )
c1 <- cars[ 1:20, list(model, mpg, cyl) ]    
c2 <- cars[ 1:10, list(model, hp, drat, wt ) ]

# straight, no duplicates in y.
expect_is( .stitch.straight( c1, c2 ), 'data.table' )

c3 <- rbind2(c2,c2)
setkey( c3, model)
expect_error( .stitch.straight( c1, c3 ) )

# Provide dup.action
attr( c3, "dup.action" ) <- function(x) x[ ! duplicated(x), ]
expect_is( .stitch.straight( c1, c3 ), 'data.table' )

# Ineffective dup.action 
attr( c3, "dup.action" ) <- identity
expect_error( .stitch.straight( c1, c3 ) )

# .stitch.straight( c1, c2 )
