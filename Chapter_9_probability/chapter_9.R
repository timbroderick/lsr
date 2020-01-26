# set directory
setwd("~/anaconda3/envs/lsr/chapter_9_probability")

# just a few useful tidbits

# load libraries
library(tidyverse)
options("scipen" = 10)


# function to return a dataframe with a set of heads/tails
# and report a summary of the result

flipsN <- function(N) {
  sample.space <- c("Heads","Tails")
  theta <- 0.5 # this is a fair coin toss
  # set up the final dataframe
  # <<- makes it a global df so that we see it outside the function
  cointoss <<- data.frame(
    flip=integer(),
    result=character(),
    stringsAsFactors=FALSE # gets rid of any problem with coercing types
  )
  # set up the df that gathers the results
  dfbind <- setNames(data.frame(matrix(ncol = 2, nrow = 1)),c("flip","result"))
  # the while loop
  x <- 0
  while ( x <= (N-1) ) {
    x <- x + 1
    flip <- sample(sample.space, 
                   size = 1, 
                   replace = TRUE, 
                   prob = c(theta, 1 - theta))
    dfbind$flip <- x
    dfbind$result <- sapply(flip,as.character)
    cointoss <<- bind_rows(cointoss,dfbind) # and in the darkness bind them
  }
  # now count and return the results
  sumthis <- cointoss %>% 
    group_by(result) %>% 
    summarize(count=n())
  return(sumthis)
}

flipsN(12)

# how about craps?

craps <- function(N=1) {
  sample.space <- c("2","3","4","5","6","7","8","9","10","11","12")
  theta <- 0.0909 # this is a fair coin toss
  # set up the final dataframe
  # <<- makes it a global df so that we see it outside the function
  cointoss <<- data.frame(
    flip=integer(),
    result=character(),
    stringsAsFactors=FALSE # gets rid of any problem with coercing types
  )
  # set up the df that gathers the results
  dfbind <- setNames(data.frame(matrix(ncol = 2, nrow = 1)),c("flip","result"))
  # the while loop
  x <- 0
  while ( x <= (N-1) ) {
    x <- x + 1
    flip <- sample(sample.space, 
                   size = 1, 
                   replace = TRUE, 
                   prob = c(theta, 1 - theta))
    dfbind$flip <- x
    dfbind$result <- sapply(flip,as.character)
    cointoss <<- bind_rows(cointoss,dfbind) # and in the darkness bind them
  }
}

craps(12)





