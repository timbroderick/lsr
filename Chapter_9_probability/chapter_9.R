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
  # calculates the likelyhood of getting this many heads vs tails
  likelyhood <- dbinom( x = sumthis$count[1], size = N, prob = 1/2 )
  print(paste("Heads probability: ",round(likelyhood*100,2),"%" ),quote=FALSE)
  return(sumthis)
}

flipsN(20)

# rbinom simulates a probability experiment n times
# in this case the result of flipping a coin (prob=1/2)
# twenty times (size) and doing that 100 times (n)
flipps <- as.data.frame( rbinom( n = 100, size = 20, prob = 1/2 ) )
colnames(flipps) <- c("flips")
flipps$set <- c(1:100)

qplot(flips,
      data=flipps,
      fill= I("cadetblue"),
      bins = 30
)

?rnorm
# same thing, but using a normal distribution
normflips <- as.data.frame( rnorm( n = 1000 ) )
colnames(normflips) <- c("flips")
normflips$set <- c(1:1000)

ggplot(normflips, aes(x = flips)) + 
  geom_histogram(aes(y =..density..),
                 bins=30, 
                 fill = "cadetblue") +
  stat_function(fun = dnorm, args = list(mean = mean(normflips$flips), sd = sd(normflips$flips)))


normal.a <- rnorm( n=1000, mean=0, sd=1 ) 
hist(normal.a)

?rchisq
chinorm <- rchisq(1000,3)
hist(chinorm)




# simple craps game

craps <- function(N=1) {
  sample.space <- c(1,2,3,4,5,6)
  theta <- 0.1667 
  # set up the final dataframe
  # <<- makes it a global df so that we see it outside the function
  dice <<- data.frame(
    dice=integer(),
    result=numeric(),
    stringsAsFactors=FALSE # gets rid of any problem with coercing types
  )
  # set up the df that gathers the results
  dfbind <- setNames(data.frame(matrix(ncol = 2, nrow = 1)),c("dice","result"))
  # the while loop
  x <- 0
  while ( x <= N ) {
    x <- x + 1
    roll <- sample(sample.space, 
                   size = 1, 
                   replace = TRUE, 
                   prob = c(theta, theta, theta, theta, theta, theta))
    dfbind$dice <- x
    dfbind$result <- roll
    dice <<- bind_rows(dice,dfbind) # and in the darkness bind them
    
  }
  print(dice)
  totl <- dice$result[1]+dice$result[2]
  print(paste("Total is",totl ),quote=FALSE)

}

craps()


