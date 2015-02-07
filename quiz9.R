library(psych)
library(car)
library(lsr)

# Read data into a dataframe called AB
setwd("~/ocw/statistics1");
data <- read.table("stats1-datafiles-Stats1.13.HW.09.txt", header = T)

# Prime: 1 = parable of the Good Samaritan; 2 = occupational effectiveness
# Haste: 1 = early, 2 = on time, 3 = late

cat("\nQ1: integer")
cat("\nQ2: Yes")
data$Haste = factor(data$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))
data$Prime = factor(data$Prime, levels = c(1,2),   labels = c("Parable", "Control")) 
model = aov(data$Helping ~ data$Haste * data$Prime)
summary(model)

cat('\nQ3: Yes')

cat('\nQ4: Yes')

cat('\nQ5: No')
TukeyHSD(model)
cat('\nQ6: 0.4')
cat('\nQ7: 0.18')
# etaSquared(model, anova = T)
A.B = read.table("stats1-datafiles-Stats1.13.HW.09.txt", header = T)
Haste = factor(A.B$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))
Prime = factor(A.B$Prime, levels = c(1,2), labels = c("Parable", "Control")) 
aov.A.B = aov(A.B$Helping ~ Haste * Prime)
summary(aov.A.B)
etaSquared(aov.A.B, anova=T)

cat('\nQ8: early')
data.H1 = subset(data, data$Haste == "Early")
data.H2 = subset(data, data$Haste == "On Time")
data.H3 = subset(data, data$Haste == "Late")
model.H1 = aov(data.H1$Helping ~ data.H1$Prime)
model.H2 = aov(data.H2$Helping ~ data.H2$Prime)
model.H3 = aov(data.H3$Helping ~ data.H3$Prime)
summary(model.H1)
summary(model.H2)
summary(model.H3)
TukeyHSD(model.H1) # only case where appealing had a significant impact
TukeyHSD(model.H2)
TukeyHSD(model.H3)

cat('\nQ9: 0.59')
etaSquared(model.H1, anova=T)

cat('\nQ10: more likely to help after being primed if early')