# set directory
setwd("~/anaconda3/envs/lsr/chapter_8_programming")

# just a few useful tidbits

# load libraries
library(tidyverse)
options("scipen" = 10)

# the while loop
x <- 0
while ( x <= 10 ) {
  x <- x + 1
  print(x)
}


# functions
quadruple <- function(x) {
  y <- x*4
  return(y)
}

quadruple(10)

# specifying a function with a default value
pow <- function( x, y = 1) {
  out <- x^y  # raise x to the power y
  return( out )
}

pow(x=15) # leaves the default for y
pow(15,10) # specifies x and y


