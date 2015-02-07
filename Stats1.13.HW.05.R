# quiz 5

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("~/ocw/statistics1");

# Read data into a dataframe called PE (physical endurance)
data <- read.table("Stats1.13.HW.04.txt", header = T)
#View(data)

# Run a regression model with salary as the outcome variable and years of experience as
# the predictor variable. What is the 95% confidence interval for the regression coefficient? 
# Type your answer exactly as it appears in R but include only two decimal places 
# (for example, if the 95% confidence interval is -1 to +1 then type -1.00 1.00)
exp_model = lm(data$salary ~ data$years)

cat('Q1: confidence interval salary ~ years exp: 4930.12 6345.48, ', confint(exp_model)[2,])

# 
cls_model = lm(data$salary ~ data$courses)
cat('\nQ2: confidence interval salary ~ courses: 560.09 872.09, ', confint(cls_model)[2,])

# Q3 Run a multiple regression model with both predictors and compare it with both the model from Question 1 and the model from Question 2. Is the model with both predictors significantly better than:
both_model = lm(data$salary ~ data$years + data$courses)
cat('\nQ3: multi regression: ')

# Q4
anova(exp_model, both_model)
anova(cls_model, both_model)
cat('significant than both')

# Run a standardized multiple regression model with both predictors. Do the confidence interval values differ from the corresponding unstandardized model?
cat('\nQ5: standardardized differs? ')
confint(both_model) # unstandardized
both_model.z = lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
confint(both_model.z)
anova(both_model, both_model.z)
cat('yes')
# set.seed(1). Now take a random subset of the original data so that N=15. 
# Is the correlation coefficient between salary and years of experience in this sample higher or lower than in the whole data set?
cat('\nQ6: subset ')
set.seed(1)
data.15 = data[sample(nrow(data),15),] # PE.20 <- PE[sample(nrow(PE), 20), ]
exp_model.15 = lm(data.15$salary ~ data.15$years)
exp_model.15
cat('cor between sal and years for data.15 ',cor(data.15)[3,2],' and data ', cor(data)[3,2])

# Take a subset of the original data from row 51 to 70. What is the percentage of variance 
# explained by a multiple regression model with both predictors 
# (Provide your result with no decimal place)
cat('\nQ7: ')
data.51_70 = data[51:70,]
both_model.51_70 = lm(data.51_70$salary ~ data.51_70$years + data.51_70$courses)
both_model.51_70
summary(both_model.51_70)