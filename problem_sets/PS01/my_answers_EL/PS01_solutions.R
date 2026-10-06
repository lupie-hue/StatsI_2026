#####################
# load libraries
# set wd
# clear global .envir
#####################

# set wd
setwd('/Users/emilialupi/Desktop/Applied statistical analysis I/GitHub/StatsI_2026/problem_sets/PS01/solutions')

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2"),  pkgTest)

#####################
# Problem 1
#####################

# Dataset
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Quick overview 
summary(y) 

# Descriptive statistics
n <- length(y)
mean_y <- sum(y)/n
var_y  <- sum((y - mean_y)^2) / (n - 1)
sd_y   <- sqrt(var_y)
se_y   <- sd_y / sqrt(n)

n; mean_y; sd_y; se_y

# Visualizing the distribution
hist(y,
     col = "violet",
     border = "white",
     main = "Histogram of IQ scores",
     xlab = "IQ scores")

plot(density(y),
     col = "violet",
     main = "Density of IQ scores",
     xlab = "IQ scores")

# -------------------------------#
# QUESTION 1
# -------------------------------#
# Find a 90% confidence interval for the average student IQ in the school 
# (As a hint, you first need the mean and SD to create a CI)

# The population SD is unknown and n is small, so we use the t distribution
# with df = n - 1 = 24. For a 90% CI we leave 5% in each tail (p = 0.95).
t_score <- qt(0.95, df = n - 1)
t_score

# CI = mean +/- critical value * SE
lower_90 <- mean_y - t_score * se_y
upper_90 <- mean_y + t_score * se_y

lower_90; mean_y; upper_90

# Built-in function check
t.test(y, conf.level = 0.90)$conf.int

# ANSWER: The 90% CI for the average student IQ is approximately (93.96, 102.92).

# -------------------------------#
# QUESTION 2
# -------------------------------#
# The school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

# "Higher than" indicates that it's going to be a one-tail test

# Is the average IQ in the school higher than 100? (α = 0.05)
# H0: mu <= 100  (the average IQ is not higher than 100)
# HA: mu > 100   (the average IQ is higher than 100)

# t = (sample mean - hypothesized mean) / SE
t_stat <- (mean_y - 100) / se_y
t_stat

# p-value: probability of a t value at least this large in the upper tail
p_val <- pt(t_stat, df = n - 1, lower.tail = FALSE)
p_val

# Built-in function check
t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

# ANSWER: t = -0.60, df = 24, p = 0.72 > 0.05 -> fail to reject H0.
# There is no evidence that the average IQ in the school is higher than 100.
# The sample mean (98.44) is below 100.

#####################
# Problem 2
#####################

# Dataset
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# Quick overview
head(expenditure)
str(expenditure)
summary(expenditure)

# Variables:
# - Y:      per capita expenditure on shelters/housing assistance
# - X1:     per capita personal income
# - X2:     residents per 100,000 that are "financially insecure"
# - X3:     people per thousand residing in urban areas
# - Region: 1 = Northeast, 2 = North Central, 3 = South, 4 = West

expenditure$Region <- factor(expenditure$Region,
                             levels = 1:4,
                             labels = c("Northeast", "North Central", "South", "West"))
table(expenditure$Region)

# -------------------------------#
# QUESTION 1
# -------------------------------#
# Plot relationships among Y, X1, X2, and X3
pairs(expenditure[, c("Y", "X1", "X2", "X3")],
      main = "Relationships among Y, X1, X2 and X3",
      pch = 16)

# What are the correlations among them?
round(cor(expenditure[, c("Y", "X1", "X2", "X3")]), 2)
# ANSWER:

# -------------------------------#
# QUESTION 2
# -------------------------------#
# Plot relationship between Y and Region
ggplot(expenditure, aes(x = Region, y = Y, fill = Region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("hotpink", "purple", "blue", "turquoise")) +
  labs(title = "Housing Assistance Expenditure by Region",
       x = "Region",
       y = "Per capita expenditure on housing assistance") +
  theme_minimal() + guides(fill = "none")

# Which region has the highest average expenditure?
# aggregate() applies mean() to Y separately for each region.
aggregate(Y ~ Region, data = expenditure, FUN = mean)

# ANSWER (approx.): the West has the highest average (88.3), followed by the
# North Central (83.9), the Northeast (79.4) and the South (69.2).
# The West also has the highest median in the boxplot.

# -------------------------------#
# QUESTION 3
# -------------------------------#
# Plot relationship between Y and X1
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(color = "hotpink") +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Relationship between Expenditure and Income",
       x = "Per capita personal income (X1)",
       y = "Per capita expenditure on housing assistance (Y)") +
  theme_minimal()

# From the graph we can see that Y and X1 have a positively linear relationship

# Adding Region variable
ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("hotpink", "purple", "blue", "turquoise")) +
  labs(title = "Relationship between Expenditure and Income, by Region",
       x = "Per capita personal income (X1)",
       y = "Per capita expenditure on housing assistance (Y)",
       color = "Region", shape = "Region") +
  theme_minimal()
