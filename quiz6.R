setwd("~/ocw/statistics1");

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called FS (Faculty Salary)
data <- read.table("stats1-datafiles-Stats1.13.HW.06.txt", header = T)
years_model = lm(data$salary ~ data$years)
summary(years_model)
cat('\n q1 sal ~ years: ', 5638)

confint(years_model)
cat('\nq2: confint 95%: ', 4930, 6345)

cat('\nq3: coeff for years in sal ~ year + courses:', 4807)
yearcourse_model = lm(data$salary ~ data$years + data$courses)
summary(yearcourse_model)

cat('\nq4: 95% confint:', 4140, 5473)
confint(yearcourse_model)

cat('\nq5: predicted diff in sal for drs lawyers for avg years and courses:', 9204)
profession.code = C(data$profession, treatment)
drlawyer_model = lm(data$salary ~ data$years + data$courses + profession.code)
summary(drlawyer_model)

cat('\nq6: diff between drs and lawyers statistically sig:', 'yes')

cat('\nq7: predicted diff between drs and teachers: ', 15903)

cat('\nq8: predicted diff statisticall sig? ', 'yes')
cat('\nq9: actual difference in mean sal between drs and teachers: ')
salary_means = tapply(data$salary, data$profession,mean)
# names(salary_means)
cat(salary_means['doctor'] - salary_means['teacher'])

yearprof_model = lm(data$salary ~ data$years + profession.code)
courseprof_model = lm(data$salary ~data$courses + profession.code)
all_model = drlawyer_model
anova(yearcourse_model,yearprof_model, courseprof_model, all_model)
anova(yearcourse_model, all_model)
anova(yearprof_model, all_model)
anova(courseprof_model, all_model)
cat('\nq10: ', 'all')