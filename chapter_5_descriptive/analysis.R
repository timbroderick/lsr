# set our working directory
setwd("~/anaconda3/envs/notebook/analysis")
# load the tidyverse for charting
library(tidyverse)
library(ggthemes)
# load the library to connect to postgress
library(RPostgreSQL)

# let's grab the data from postgres
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
con <- dbConnect(drv, dbname = "aranalysis",
                 host = "localhost", port = 5432,
                 user = "tbroderick")


#create the sql command
paindex <- "SELECT * FROM paindex;"

# grab the data and put into a dataframe
df <- dbGetQuery(con, paindex)

summary(df)

# now let's do some quick plots

qplot(me2017school,
      data=df,
      bins = 80
)

qplot(lowinc2017,me2017school,
      data=df,
      xlab="M/E", 
      ylab="low income") + 
  stat_smooth(method="lm")

# let's see summary stats for simple regression
fit1 <- lm(lowinc2017 ~ me2017school, data = df)
summary(fit1)

# let's see summary stats for a multiple regression
#https://www.statmethods.net/stats/regression.html
fit2 <- lm(lowinc2017 ~ me2017school + me2017schapp, data = df)
summary(fit2)

# save fit1 to a file
sink("csv/fit1.txt")
print(summary(fit1))
sink()  # returns output to the console

# save fit2 to a file
sink("csv/fit2.txt")
print(summary(fit2))
sink()  # returns output to the console

summary(fit1)

# now let's index each value
# first we find the mean
meavg <- round(mean(df$me2017school), 1)
# then the standard deviation
mestd <- round(sd(df$me2017school), 2)
# let's print in the console
cat("ME average is ",meavg,"\n","Standard deviation is ",mestd)

# let's figure out the rest of our parameters
ssize <- nrow(df) # number of rows
# find the standard error at 95%
sterror <- round(mestd/sqrt(length(df$me2017school)) * 1.98,2)
# sqrt computes the square root
# the whole equation is
# standard deviation divided by
# the square root of the number of items
# times 1.98
sterror

# another way
# https://www.cyclismo.org/tutorial/R/confidence.html
sterroralt <- qnorm(0.975)*mestd/sqrt(ssize)
help(qnorm)
sterroralt

# so now we can find our lower and upper bounds
low95 <- round(meavg - sterror,2)
high95 <- round(meavg + sterror,2)


# now let's put it all together
cat("ME | Sample | StDev | Error | 95% Low | 95% High")
cat( meavg," | ",ssize," | ",mestd," | ",sterror," | ",low95," | ",high95 )

# let's get this into our data set
df$mean <- meavg
df$std <- mestd
df$error <- sterror
df$low95 <- low95
df$high95 <- high95
df$zscore <- with(df, round( ( me2017school - meavg) / mestd,2 ) )

head(df)

# let's save all this to postgress

# first, delete the existing table
drop <- "DROP TABLE IF EXISTS paindex;"
dbGetQuery(con, drop)
# tests to see if it's there
dbExistsTable(con, "paindex")

#create the sql command
paindex <- "CREATE TABLE paindex (
schid varchar(25),
schname varchar(50),
distname varchar(50),
me2017school numeric,
me2017schapp numeric,
lowinc2017 numeric,
mean numeric,
std numeric,
error numeric,
low95 numeric,
high95 numeric,
zscore numeric
);"
# make the table
dbGetQuery(con, paindex)
# tests to see if it's there
dbExistsTable(con, "paindex")

# now let's append our x data into the new table
dbWriteTable(con, "paindex", df, append = TRUE, row.names = FALSE)

# let's get the data to test
paindex <- "SELECT * FROM paindex;"

# grab the data and put into a dataframe
testing <- dbGetQuery(con, paindex)

summary(testing)

# disconnect and unload driver when done
dbDisconnect(con)
# in case there are any other open connections
lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
dbUnloadDriver(drv)

# finally, let's take a look at our index
qplot(zscore,
      geom = "histogram",
      data=testing,
      #colour = I("black"), 
      fill = I("blue"),
      xlab = "zscore", 
      ylab = "Count",
      bins = 100
)