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
lapply(c("car", "stargazer"),  pkgTest)

# set wd for current folder
setwd('/Users/emilialupi/Desktop/Applied statistical analysis I/GitHub/StatsI_2026/problem_sets/PS04/my_answers')

# function to save stargazer output to a .tex file
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = FALSE)
}

#####################
# Problem 1
#####################
data(Prestige)
help(Prestige)

# a) new variable professional by recoding the variable type so that:
# professional = 1
# blue and white collar workers = 0
table(Prestige$type, useNA = "ifany")
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$professional, useNA = "ifany")

# b) linear model with:
# prestige as outcome
# income, professional, and the interaction of the two as predictors

# i remove the NAs from the dataset
prestige_clean <- Prestige[complete.cases(Prestige), ]
# income * professional == income + professional + income:professional
model <- lm(prestige ~ income * professional, data = prestige_clean)
summary(model)

output_stargazer("model_prestige.tex", model, 
                 title = "Regression of prestige on income and professional",
                 label = "tab:model_prestige", table.placement = "H",
                 covariate.labels = c("Income", "Professional", "Income x Professional"), 
                 dep.var.labels = "Prestige")

# c) prediction equation
# prestige_hat = 21.142 + 0.00317*income + 37.781*professional - 0.00233*(income*professional)

# d) interpretation coefficient income
# for blue and white collar occupations, each one-unit increase in income is 
# associated with a 0.00317 increase in the prestige score for occupation on average

# e) interpretation coefficient professional
# when income is 0, professional occupations have a prestige score 37.781 points 
# higher than blue and white collar occupations, on average.

# coefficients from the model
b0 <- summary(model)$coefficients[1]   # intercept
b1 <- summary(model)$coefficients[2]   # income
b2 <- summary(model)$coefficients[3]   # professional
b3 <- summary(model)$coefficients[4]   # income:professional

# f) marginal effect of income 
# when the variable professional takes the value of 1
# prestige_hat = (b0 + b2) + (b1 + b3)*income
# so the marginal effect of income is b1 + b3
me_income_prof <- b1 + b3
me_income_prof

# effect of a 1,000 dollar increase in income
me_income_prof * 1000

# plug into the predicted equation
yhat_6000 <- b0 + b1*6000 + b2*1 + b3*(6000*1)
yhat_6000
yhat_7000 <- b0 + b1*7000 + b2*1 + b3*(7000*1)
yhat_7000
yhat_7000 - yhat_6000

# g) marginal effect of professional jobs 
# when the variable income takes the value of 6,000
# prestige_hat = b0 + b1*income + (b2 + b3*income)*professional
# so the marginal effect of professional is b2 + b3*income
me_prof_6000 <- b2 + b3*6000
me_prof_6000

# plug into the prediction equation
yhat_nonprof <- b0 + b1*6000 + b2*0 + b3*(6000*0)
yhat_nonprof
yhat_prof    <- b0 + b1*6000 + b2*1 + b3*(6000*1)
yhat_prof
yhat_prof - yhat_nonprof


# PLOT: different intercept and different slope for each group
pdf("plot_g.pdf")
plot(prestige_clean$income, prestige_clean$prestige,
     xlab = "Income", ylab = "Prestige",
     pch = 16,
     col = ifelse(prestige_clean$professional == 1, "maroon", "royalblue"))
abline(b0, b1, col = "royalblue")             # professional = 0
abline(b0 + b2, b1 + b3, col = "maroon")      # professional = 1
legend("bottomright",
       legend = c("Blue and white collar", "Professional"),
       col = c("royalblue", "maroon"), pch = 16)
dev.off()


#####################
# Problem 2
#####################

# results from the regression table
b_assigned <- 0.042   # coefficient for precincts assigned lawn signs
se_assigned <- 0.016  # standard error for lawn signs
b_adjacent <- 0.042   # coefficient for precincts adjacent to lawn signs
se_adjacent <- 0.013  # standard error for adjacent precincts
b_constant <- 0.302   # coefficient for constant term
se_constant <- 0.011  # standard error for constant term
n <- 131              # number of precincts
k <- 2                # number of predictors (not counting the constant)
df <- n - k - 1       # degrees of freedom
R2 <- 0.094           # coefficient of determination

# a) hypothesis test
# H0: beta_1 = 0   vs   Ha: beta_1 != 0   (alpha = 0.05)
t_assigned <- (b_assigned - 0) / se_assigned
t_assigned
p_assigned <- 2 * pt(-abs(t_assigned), df = df)
p_assigned 
# 0.0097 < 0.05 so we reject the H0

# b) hypothesis test
# H0: beta_2 = 0   vs   Ha: beta_2 != 0   (alpha = 0.05)
t_adjacent <- (b_adjacent - 0) / se_adjacent
t_adjacent
p_adjacent <- 2 * pt(-abs(t_adjacent), df = df)
p_adjacent 
# 0.0016 < 0.05 so we reject the H0

# c) interpretation coefficient for the constant term
b_constant
# Average vote share for Cuccinelli in precincts that were not assigned 
# lawn signs and were not adjacent to precincts with lawn signs 
# (both variables = 0).
# In these precincts, Cuccinelli got about 30.2% of the vote, on average. 

# d) evaluation model fit
R2
# The R-squared is 0.094, so the model explains only about 9.4% of the 
# variation in Cuccinelli's vote share across precincts. The other 90.6% 
# is explained by factors not in the model.
