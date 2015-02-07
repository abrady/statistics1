# Welcome to assignment 10! This week we are going to work on an example at the intersection of 
# decision-making and global warming. The simulated dataset includes a dependent variable:
# change
# for a list of 27 countries. Change indicates whether these countries are willing to take action
# now against global warming, or if they would rather wait and see 
# (1 = act now, 0 = wait and see). 
#
# Predictors include: 
# median age (age), 
# education index (educ),
# gross domestic product (gdp),
# and CO2 emissions (co2).

# Read data into a dataframe called AB
setwd("~/ocw/statistics1");
data <- read.table("stats1-datafiles-Stats1.13.HW.10.txt", header = T)
summary(data[data$change == 1,])

# Load packages
library(psych)
library(aod)
library(QuantPsyc)

cat("\nQ1: 35.78")
describeBy(data$age, data$change=="1") # mean, not median

cat('\nQ2: age,educ')
colnames(data)
model.change = glm(as.factor(data$change) ~ data$age + data$educ + data$gdp + data$co2, family = binomial)
summary(model.change)

cat('\nQ3: all of the above')

cat('\nQ4: -31.17 -3.03')
confint(model.change) # CIs using profiled log-likelihood (default for logistic models)

cat('\nQ5: 0.09 0.65')
confint.default(model.change)

cat('\nQ6:',round(model.change$null.deviance - model.change$deviance,2))

cat('\nQ7:',round(model.change$df.null - model.change$df.residual))

cat('\nQ8: yes')
with(model.change, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE))

cat('\nQ9: yes')
library(aod)
colnames(data)
wald.test(b = coef(model.change), Sigma = vcov(model.change), Terms = 4)

cat('\nQ10: 81')
ClassLog(model.change, data$change)

