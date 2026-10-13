# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd('/Users/emilialupi/Desktop/Applied statistical analysis I/GitHub/StatsI_2026/tutorials/Week 5')

# Check working directory
getwd()

# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("../../datasets/fictional_data.csv")
View(df)

# Create a scatter plot 
plot(df$edu, df$income)

# Calculate correlation
r <- cor(df$edu, df$income)

# Add correlation to scatter plot
plot(df$edu, df$income)
text(x = max(df$edu, na.rm = TRUE) * 0.1,   
     y = max(df$income, na.rm = TRUE) * 0.9,
     labels = sprintf("Correlation = %.4f", r),
     adj = 0)

# Improve visualization with detailed axis and main labels, save as PDF
pdf("scatter_plot_cor.pdf")
plot(df$edu, df$income,
     xlab = "education (years)",
     ylab = "income (Euro)",
     main = "Relationship between education and income")
text(x = max(df$edu, na.rm = TRUE) * 0.1,   
     y = max(df$income, na.rm = TRUE) * 0.9,
     labels = sprintf("Correlation = %.4f", r),
     adj = 0)
dev.off()

# Run a t-test for the correlation coefficient
cor.test(df$edu, df$income)

# Check p-value
sprintf("%.20f", 7.52e-07)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# We can reject the null hypothesis 

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
summary(lm(income ~ edu, data=df))

# Save model as object
model <- lm(income ~ edu, data=df)

# Output interpretation:
#  - Intercept (b0): with no education, the avg income is of €976.16
#  - Slope (b1): with a one-unit increase in education, the income will increase
#                on avg of €250.64

# Conduct t-test for the slope of a regression line
250.64/33.06
# Check p-value
sprintf("%.20f", 2.17e-06)

# Create a residual plot
model.res <- resid(model)
model.res

plot(df$edu, model.res,
     ylab = "residuals",
     xlab = "education",
     abline(0,0))

# homoskedastic pattern that we want (residuals seems random)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients
confint(model, level=0.95)
confint(model, level=0.99)
# non c'è lo 0 compreso negli intervalli so we fail to reject the H0

# Create a scatter plot w/ educate and income, add regression line

# What is the prediction equation?
# predicted_income = 976.15 + 250.64 * edu

# Make predictions for first observation in df
predict(model, newdata = data.frame(edu = c(0,1,2,3,4,5,6,7,8)))
predict(model, newdata = data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit = TRUE)

# when the x are beyond what my dataset have, there is a risk

# Make predictions for a range of x values (specifically min to max of education)

# Add standard errors for your predicted values

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
# Rembmer: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2