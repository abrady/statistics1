# 

setwd("~/ocw/statistics1");

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
#install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

data <- read.table("stats1-datafiles-Stats1.13.HW.07.txt", header = T)
cat('\nQ1:', round(cor(data$happy, data$extra),2))
cat('\nQ2:', round(cor(data$extra, data$diverse),2))
cat('\nQ3:', round(cor(data$diverse, data$happy),2))

cat('\nQ4: 4')
model_extra = lm(data$happy ~ data$extra)
summary(model_extra)

# What percentage of variance in happiness is explained by a model with both 
# extraversion and diversity of life experience as predictors?
cat('\nQ5: 7')
model_extradiv = lm(data$happy ~ data$extra + data$diverse)
summary(model_extradiv)

# 95% confidence interval for the regression coefficient for extraversion when it is the only 
# predictor of happiness?
cat('\nQ6: .07 .48')
confint(model_extra)

# 95% confidence interval for the regression coefficient for extraversion when it and 
# diversity of life experience are both predictors of happiness?
cat('\nQ7: .02 .43')
confint(model_extradiv)

# What is the unstandardized regression estimate of the indirect effect?
cat('\nQ8: 0.05')
describeBy(data, data$diverse) # indicates
model_sobel = sobel(data$extra, data$diverse, data$diverse)
model_sobel

cat('\nQ9: 1.88')

cat('\nQ10: partial')