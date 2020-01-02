# set directory
setwd("~/anaconda3/envs/lsr/chapter_5_descriptive")

# load libraries
library(tidyverse)
#library('readxl')
library("RPostgreSQL")
options("scipen" = 10)

# load data
load("aflsmall.Rdata")

# afl.finalists are factors
# afl.margins are numbers
# let's make our typing easier
df <- afl.margins

hist(df, breaks=10, main="Histogram", col="green")

# find summary stats for first five
mean( df[1:5] )
median( df[1:5] )

# trimmed mean (10% trimmed mean = dropping 10% from top and bottom)
dataset <- c( -15,2,3,4,5,6,7,8,9,12 )
mean(dataset)
mean( dataset, trim = .1) # 10% trimmed mean
mean( dataset, trim = .05) # 5% etc
?mean

# making our typing easier
dfin <- as.data.frame(afl.finalists) 

colnames(dfin) <- c("places")
unique(dfin$places)
# there are 17 unique places
table(dfin)
# weird that Fitzroy is included but has zero entries?

head(dfin)

# and gets dropped in group_by
dfsum <- dfin %>% 
  group_by(places) %>% 
  summarize(count=n()) %>% 
  arrange(-count) # - is order descending

# mode of dfin = top one
head(dfsum,1)

# let's do this for margin
df <- as.data.frame(df)
colnames(df) <- c("margins")


# and gets dropped in group_by
dfsum2 <- df %>% 
  group_by(margins) %>% 
  summarize(count=n()) %>% 
  arrange(-count) # - is order descending

# mode of dfin = top one
head(dfsum2,1)

# --------------------------
# calculating variability
max(df$margins)
min(df$margins)
# range does both
range(df$margins)
# then there's summary
summary(df)

# IQR
quantile(df$margins, c(.25,.50,.75))
IQR(df$margins)
# basically, calculates the .25 and .75, then subtracts them
quantile(df$margins,.75) - quantile(df$margins,.25)

# let's get everything and find some outliers
dfbind <- setNames(data.frame(matrix(ncol = 11, nrow = 1)), # of columns must match
                   c("count","q25","q50","q75","iqr","cut_off","lower","upper","mean","count_lower","count_upper"))
dfbind$count <- as.integer(nrow(df))
dfbind$q25 <- round( quantile(df$margins, .25, na.rm=TRUE),2)
dfbind$q50 <- round( quantile(df$margins, .50, na.rm=TRUE),2)
dfbind$q75 <- round( quantile(df$margins, .75, na.rm=TRUE),2)
# now find the interquartile range
dfbind$iqr <- IQR(df$margins)
# calculate the outlier cutoff, which is 1.5 of the iqr
dfbind$cut_off <- round( dfbind$iqr * 1.5,2)
# that means anything below the lower bound or higher than the upper bound is considered an outlier
dfbind$lower <- dfbind$q25 - dfbind$cut_off
dfbind$upper <- dfbind$q75 + dfbind$cut_off
dfbind$mean <- round( mean(df$margins, na.rm=TRUE) ,2)
dfbind$median <- round( median(df$margins, na.rm=TRUE) ,2)
dfbind$count_lower <- as.integer( nrow( filter(df,margins <= dfbind$lower) ) )
dfbind$count_upper <- as.integer( nrow( filter(df,margins > dfbind$upper) ) )



# Mean absolute deviation
# average distance between each data point and the mean
# The mean absolute deviation show the amount of deviation (variation) 
# that occurs around the mean score.
# We use the absolute value of the numbers because negatives
abs(-5) # would tend to cancle out positive numbers

# example
X <- c(56,31,56,8,32)   # enter the data
Xmean <- mean( X )       # step 1. the mean of the data
AD <- abs( X - Xmean )   # step 2. find the difference from mean for each number
                        # making sure all numbers are positive
AAD <- mean( AD )        # step 3. find the average of those differences
AAD # average (mean) absolute deviation
# or
mean ( abs( X - mean( X ) ) )
# so for margins
mean( abs(df$margins - mean(df$margins)) )

# variance
mean( (df$margins - mean(df$margins) )^2)
# Instead of absolute value, variance squares (^2) the results to get positive values
# a large number means the values are very spread out
# a smaller number means the values are closer to the mean
# variance is also used for finding sd
sqrt( var(df$margins) )
sd(df$margins) # square root of the variance

# median absolute deviation is the same but with median
# this is handy to identify outliers when the mean is overly affected by them
median( abs(df$margins - median(df$margins)) )
# the mad functionallows a consistency constant. 
# This modification will ensure that for large samples the MAD provides a good 
# estimate of the standard deviation (more formally, the MAD becomes a 
# consistent estimator of the population standard deviation)
# https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/s
mad(df$margins, constant = 1.4826)
# 1.4826 assumes that the underlying distribution is normal
# if we can't assume, set constant to 1 if 
?mad

# In sum
# Range. Gives you the full spread of the data. It’s very vulnerable to outliers, 
#    and as a consequence it isn’t often used unless you have good reasons to care 
#    about the extremes in the data.
# Interquartile range. Tells you where the “middle half” of the data sits. 
#    It’s pretty robust, and complements the median nicely. This is used a lot.
# Mean absolute deviation. Tells you how far “on average” the observations are 
#    from the mean. It’s very interpretable, but has a few minor issues 
#    that make it less attractive to statisticians than the standard deviation. 
#    Used sometimes, but not often.
# Variance. Tells you the average squared deviation from the mean. 
#    It’s mathematically elegant, and is probably the “right” way to describe 
#    variation around the mean, but it’s completely uninterpretable because it 
#    doesn’t use the same units as the data. Almost never used except as a 
#    mathematical tool; but it’s buried “under the hood” of a very large number of 
#    statistical tools.
# Standard deviation. This is the square root of the variance. 
#    It’s fairly elegant mathematically, and it’s expressed in the same units as the 
#    data so it can be interpreted pretty well. In situations where the mean is the 
#    measure of central tendency, this is the default. This is by far the most 
#    popular measure of variation.
# Median absolute deviation. The typical (i.e., median) deviation from the median 
#    value. In the raw form it’s simple and interpretable; in the corrected form 
#    it’s a robust way to estimate the standard deviation, for some kinds of data sets. 
#    Not used very often, but it does get reported sometimes.


# skewness and kurtosis
# https://www.r-bloggers.com/measures-of-skewness-and-kurtosis/
# https://cran.r-project.org/web/packages/moments/index.html
# install.packages("moments")
library(moments)
skewness(df$margins)
kurtosis(df$margins)

# returns different values than the lsr package apparently

# summary stats
blowouts <-  afl.margins > 50
summary(blowouts)


load("clinicaltrial.Rdata")
summary(clin.trial)

meanerr <- function(x) sd(x)/sqrt(length(x)) # function for computing standard error of the mean

# get describe stats
NROW(clin.trial$mood.gain)
round( mean(clin.trial$mood.gain),2)
round( sd(clin.trial$mood.gain),2)
round( median(clin.trial$mood.gain),2)
round( mean(clin.trial$mood.gain,trim=.1),2)
round( mad(clin.trial$mood.gain),2)
round( min(clin.trial$mood.gain),2)
round( max(clin.trial$mood.gain),2)
round( max(clin.trial$mood.gain) - min(clin.trial$mood.gain),2)
round( skewness(clin.trial$mood.gain),2)
round( kurtosis(clin.trial$mood.gain),2)
round( meanerr(clin.trial$mood.gain),2)

#alternately, let's try the psych packate
#install.packages("psych")
library(psych)
# kind of group by summary stats
describeBy( x=clin.trial, group=clin.trial$therapy )
# could also do
by( data=clin.trial, INDICES=clin.trial$therapy, FUN=summary )
# where indices is the group_by indicator


# What if you have multiple grouping variables? Suppose, for example, you would like to 
# look at the average mood gain separately for all possible combinations of drug and therapy. 
# It is actually possible to do this using the by() and describeBy() functions, but I usually
# find it more convenient to use the aggregate() function in this situation. 
# There are three arguments that you need to specify. The formula argument is used to 
# indicate which variable you want to analyse, and which variables are used to specify the 
# groups. For instance, if you want to look at mood.gain separately for each possible 
# combination of drug and therapy, the formula you want is mood.gain ~ drug + therapy. 
# The data argument is used to specify the data frame containing all the data, and the 
# FUN argument is used to indicate what function you want to calculate for each group 
# (e.g., the mean). So, to obtain group means, use this command:

aggregate( formula = mood.gain ~ drug + therapy,  # mood.gain by drug/therapy combination
           data = clin.trial,                     # data is in the clin.trial data frame
           FUN = mean                             # print out group means
)


# standardized scores or z scores
# analysis and analysis 2 in this folder explores z scores in depth

# correlation
load("parenthood.Rdata")
summary(parenthood)
# Pearson’s correlation coefficient calculates the strength of a linear correlation 
# between two sets
# -1 = perfectly negative, 0 = no correlation, 1 = perfectly positive
# cor calculates this
cor( x = parenthood$dan.sleep, y = parenthood$dan.grump )
# doing the whole data frame produces a matrix
cor( x = parenthood )
# judging the correlation
C("correlation","strength","direction")
c("-1.0 to -0.9" ,"Very strong", "Negative")
c("-0.9 to -0.7", "Strong", "Negative")
c("-0.7 to -0.4", "Moderate", "Negative")
c("-0.4 to -0.2", "Weak", "Negative")
c("-0.2 to 0","Negligible", "Negative")
c("0 to 0.2","Negligible", "Positive")
c("0.2 to 0.4", "Weak", "Positive")
c("0.4 to 0.7", "Moderate", "Positive")
c("0.7 to 0.9", "Strong", "Positive")


