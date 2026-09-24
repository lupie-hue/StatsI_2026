# remove objects
rm(list=ls())
# detach all libraries 
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
sum(y)

# (2) find mean of y using your "own" function
sum(y)/length(y)

# now do the same thing, but faster using the built-in R function
mean(y)

# (3) find sum of demeaned values
sum_demeaned_y <- sum(y - mean_y)

# (4) calculate sum of squared error
sum_squared_error <- sum((y - mean_y)^2)

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 
sort(quantilesVec)
median_vec <- median(quantilesVec)

# (2) calculate quantiles
summary(quantilesVec)

quantile(quantilesVec, c(0.25, 0.5, 0.75))

quantile(quantilesVec, c(0.25, 0.5, 0.75), type = 1)

boxplot(quantilesVec)

# (3) make a histogram of state median income
state.x77[,2]

pdf("median_income_hist.pdf")
hist(state.x77[,2], main = "Distribution of state median income", xlab = "Median income")
dev.off()

# remember to save your plot as a pdf