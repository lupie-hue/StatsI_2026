#####################
# load libraries
# set wd
# clear global .envir
#####################

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
lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd('/Users/emilialupi/Desktop/Applied statistical analysis I/GitHub/StatsI_2026/problem_sets/PS03/my_answers')

# function to save stargazer output to a .tex file
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = FALSE)
}

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#####################
# Problem 1
#####################

# a) regression where: 
# outcome variable = voteshare 
# explanatory variable = difflog

# use lm function to run regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)

output_stargazer("regression_q1.tex", model1,
                 title = "Regression of incumbent's vote share on difference in spending",
                 label = "tab:regression_q1",
                 dep.var.labels = "Incumbent's vote share (voteshare)",
                 covariate.labels = c("Difference in spending (difflog)", "Constant"),
                 digits = 3, header = FALSE, table.placement = "H")

# b) scatterplot + regression line
pdf("plot_q1.pdf")
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Difference in campaign spending (difflog)",
     ylab = "Incumbent's vote share",
     main = "Vote share and spending difference",
     pch = 16, cex = 0.5, col = "darkred")
abline(model1, col = "darkblue", lwd = 2)
dev.off()

# c) save residuals in separate object
resid_1 <- residuals(model1)
summary(resid_1)

output_stargazer("resid_q1.tex", t(as.matrix(summary(resid_1))),
                 title = "Summary of the residuals of Question 1 (resid\\_1)",
                 label = "tab:resid_q1",
                 summary = FALSE, rownames = FALSE,
                 digits = 4, header = FALSE, table.placement = "H")

# d) prediction equation
# voteshare_hat = 0.579 + 0.042 * difflog

#####################
# Problem 2
#####################

# a) regression where: 
# outcome variable = presvote 
# explanatory variable = difflog

# use lm function to run regression
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)

output_stargazer("regression_q2.tex", model2,
                 title = "Regression of presidential vote share on difference in spending",
                 label = "tab:regression_q2",
                 dep.var.labels = "Vote share of the president's party (presvote)",
                 covariate.labels = c("Difference in spending (difflog)", "Constant"),
                 digits = 3, header = FALSE, table.placement = "H")

# b) scatterplot + regression line
pdf("plot_q2.pdf")
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "Difference in campaign spending (difflog)",
     ylab = "Vote share of the president's party (presvote)",
     main = "Presidential vote share and spending difference",
     pch = 16, cex = 0.5, col = "darkred")
abline(model2, col = "darkblue", lwd = 2)
dev.off()

# c) save residuals in separate object
resid_2 <- residuals(model2)
summary(resid_2)

output_stargazer("resid_q2.tex", t(as.matrix(summary(resid_2))),
                 title = "Summary of the residuals of Question 2 (resid\\_2)",
                 label = "tab:resid_q2",
                 summary = FALSE, rownames = FALSE,
                 digits = 4, header = FALSE, table.placement = "H")

# d) prediction equation
# presvote_hat = 0.508 + 0.024 * difflog

#####################
# Problem 3
#####################

# a) regression where: 
# outcome variable = voteshare
# explanatory variable = presvote

# use lm function to run regression
model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)

output_stargazer("regression_q3.tex", model3,
                 title = "Regression of incumbent's vote share on presidential vote share",
                 label = "tab:regression_q3",
                 dep.var.labels = "Incumbent's vote share (voteshare)",
                 covariate.labels = c("Vote share of the president's party (presvote)", "Constant"),
                 digits = 3, header = FALSE, table.placement = "H")

# b) scatterplot + regression line
pdf("plot_q3.pdf")
plot(inc.sub$presvote, inc.sub$voteshare,
     xlab = "Vote share of the president's party (presvote)",
     ylab = "Incumbent's vote share (voteshare)",
     main = "Incumbent vote share and presidential vote share",
     pch = 16, cex = 0.5, col = "darkred")
abline(model3, col = "darkblue", lwd = 2)
dev.off()

# c) prediction equation
# voteshare_hat = 0.441 + 0.388 * presvote

#####################
# Problem 4
#####################

# a) regression where: 
# outcome variable = residual1
# explanatory variable = residual2
model4 <- lm(resid_1 ~ resid_2)
summary(model4)

output_stargazer("regression_q4.tex", model4,
                 title = "Regression of the residuals of Question 1 on the residuals of Question 2",
                 label = "tab:regression_q4",
                 dep.var.labels = "Residuals of Question 1 (resid\\_1)",
                 covariate.labels = c("Residuals of Question 2 (resid\\_2)", "Constant"),
                 digits = 3, header = FALSE, table.placement = "H")

# b) scatterplot + regression line
pdf("plot_q4.pdf")
plot(resid_2, resid_1,
     xlab = "presvote not explained by difflog",
     ylab = "voteshare not explained by difflog",
     main = "Residuals of Problem 1 and Problem 2",
     pch = 16, cex = 0.5, col = "darkred")
abline(model4, col = "darkblue", lwd = 2)
dev.off()

# c) prediction equation
# resid_1_hat = 0 + 0.257 * resid_2 

#####################
# Problem 5
#####################

# a) regression where: 
# outcome variable = voteshare
# explanatory variables = difflog + presvote
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5)

output_stargazer("regression_q5.tex", model5,
                 title = "Regression of incumbent's vote share on difference in spending and presidential vote share",
                 label = "tab:regression_q5",
                 dep.var.labels = "Incumbent's vote share (voteshare)",
                 covariate.labels = c("Difference in spending (difflog)",
                                      "Vote share of the president's party (presvote)",
                                      "Constant"),
                 digits = 3, header = FALSE, table.placement = "H")

# b) prediction equation
# voteshare_hat = 0.449 + 0.036 * difflog + 0.257 * presvote



