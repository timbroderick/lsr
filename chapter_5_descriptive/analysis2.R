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

# disconnect and unload driver when done
dbDisconnect(con)
# in case there are any other open connections
lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
dbUnloadDriver(drv)


# now let's do some quick plots
# histogram of m/e scores
qplot(me2017school,
      data=df,
      fill = I("blue"),
      bins = 80
)


# histogram of the z scores
qplot(zscore,
      geom = "histogram",
      data=testing,
      #colour = I("black"), 
      fill = I("blue"),
      xlab = "zscore", 
      ylab = "Count",
      bins = 100
)

# scatter plot of scores
qplot(lowinc2017,me2017school,
      data=df,
      xlab="M/E", 
      ylab="low income") + 
  stat_smooth(method="lm")


# let's get more nuanced
# first, let's create a column we can factor
df$chi <- ifelse(df$distname == "City of Chicago SD 299","Chicago","Rest of state")

summary(df)

# now chart

p <- ggplot(df) +
  aes(x=lowinc2017, 
      y=me2017school
  ) + 
  geom_point(stat="identity", 
             size = 2,
             shape=21,
             alpha=I(.4),
             aes(
               color=factor(chi),
               fill=factor(chi),
               text = paste("school:", schname)
               )
  ) +
  geom_smooth(method=lm)

# let's make that interactive
library(plotly)
p <- ggplotly(p)

p

# Let's get just Chicago scores
# https://www.statmethods.net/management/subset.html
dfchi <- df[ which(df$chi == "Chicago"),]
summary(dfchi)

# or

dfnew <- subset(df, chi == "Chicago",
                  select=c(schid, schname, distname, me2017school, me2017schapp, lowinc2017, zscore, chi))
summary(dfnew)


# A few more handy things
# change a string (in this case a period) in cells to NA so the columns will be recognized as numeric
df <- read.csv("data.csv", header=T, na.strings=c(".","NA"))

# merging, left join
df <- merge(x = thing1, y = thing2, by="ID", all.x = TRUE)
# https://www.statmethods.net/management/merging.html
# https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/merge
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/merge.html


