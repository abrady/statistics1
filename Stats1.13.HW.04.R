# Statistics One, 2013, Lab 4

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("~/ocw/statistics1");


# Load packages
# library(psych)

# 2 decimals
options(digits=2)

# Read data into a dataframe called PE (physical endurance)
data <- read.table("Stats1.13.HW.04.txt", header = T)
#View(data)

# Salary can be influenced by many variables. Among these, 
# - years of professional experience and 
# - total courses completed in college are critical.
# 
# This week we test this hypothesis with a simulated dataset including an 
# outcome variable, salary, and two predictors, years of experience and courses
# completed. Here are a few questions based on what was covered in the lectures 
# and the lab. Have fun!
cat("\nQ1: cor between salary and years:",cor(data$salary,data$years))
cat("\nQ2: cor between salary and courses completed:",cor(data$salary,data$courses))

cat("\nQ3: percentage of variance explained in a regression model with salary as the outcome variable and professional experience as the predictor variable:")
exp_model = lm(data$salary ~ data$years)
cat(capture.output(summary(exp_model)),sep='\n')

cat("\nQ4: percentage of variance explained in a regression model with salary as the outcome variable and professional experience as the predictor variable:")
cls_model = lm(data$salary ~ data$courses)
cat(capture.output(summary(cls_model)),sep='\n')

# plot these two
par(mfrow=c(2,1))
plot(data$salary ~ data$years, main = "Scatterplot", ylab = "salary", xlab = "years exp")
abline(exp_model, col="blue")
plot(data$salary ~ data$courses, main = "Scatterplot", ylab = "class", xlab = "years exp")
abline(cls_model, col="blue")

clsexp_model = lm(data$salary ~ data$years + data$courses)
#std_clsexp_model = lm(data$salary ~ scale(data$courses) + scale(data$salary))
cat('\n: Q5: ',capture.output(summary(std_clsexp_model)),sep='\n')
cat('\n: Q6: std regression coeff for salary against years: ')
scaled_exp_model = lm(scale(data$salary) ~ scale(data$years))
cat(capture.output(summary(scaled_exp_model)),sep='\n')

cat('\n: Q7: std regression coeff for salary against classes: ')
scaled_cls_model = lm(scale(data$salary) ~ scale(data$courses))
cat(capture.output(summary(scaled_cls_model)),sep='\n')

cat('\nQ8: mean of the salary distribution predicted by the model including both years of professional experience and courses completed as predictors:');
data$predicted = fitted(clsexp_model)
cat(mean(data$predicted))

cat('\nQ9: mean of residuals: ')
data$e = resid(clsexp_model)
cat(round(mean(data$e),0))

cat('\nQ10: residuals evenly distributed?')
hist(data$e)


# hypothesis: red cars get twice as many tickets as blue cars
cars = data.frame(color=c('red','blue'), num_cars=c(100,150), num_tickets=c(90, 50))
# add our hypothesis data
cars$expected = c(90, .9*.5*150)
# now, what is the chance of seeing our sample given our hypothesis data:

