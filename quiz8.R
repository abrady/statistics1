# stats1-datafiles-Stats1.13.HW.02

# Read data into a dataframe called wm
setwd("~/ocw/statistics1");
data = read.table("stats1-datafiles-Stats1.13.HW.02.txt", header = T)
# View(data)
# Using a dependent t-test, is the difference between pre and post-test scores significant?
cat('\nQ1: yes')
data.pre = subset(data, data$time == "pre")
data.post = subset(data, data$time == "post")
t.test(data.post$SR, data.pre$SR, paired = T)

# Create subsets for each training condition. Which group shows no difference between pre and post-test scores?
cat('\nQ2: PE')
DS.pre = subset(data, data$time == "pre" & data$condition == 'DS')
DS.post = subset(data, data$time == "post" & data$condition == 'DS')
DS.t = t.test(DS.post$SR, DS.pre$SR, paired=T) # .00005

PE.pre = subset(data, data$time == "pre" & data$condition == 'PE')
PE.post = subset(data, data$time == "post" & data$condition == 'PE')
PE.t = t.test(PE.post$SR, PE.pre$SR, paired=T) # .0625

WM.pre = subset(data, data$time == "pre" & data$condition == 'WM')
WM.post = subset(data, data$time == "post" & data$condition == 'WM')
WM.t = t.test(WM.post$SR, WM.pre$SR, paired=T) # .0625

# Which training group shows the largest effect size for the difference pre-test to post-test?
cat('\nQ3: DS')
# DS.t has the smallest P value, feel like there is more to this question...

# Reshape the data into a wide format, and create a new variable for gain score. 
# Now subset the new dataframe based on the training conditions. Which comparison between
# training conditions does not show a significant difference?
cat('\nQ4: None')
library(reshape)
data.wide = cast(data, subject+condition~time) 
data.wide$gain = data.wide$post - data.wide$pre 
wm.wide = subset(data.wide, data.wide$condition=="WM") 
pe.wide = subset(data.wide, data.wide$condition=="PE") 
ds.wide = subset(data.wide, data.wide$condition=="DS")
t.test(wm.wide$gain, pe.wide$gain, var.equal=T)
t.test(wm.wide$gain, ds.wide$gain, var.equal=T)
t.test(ds.wide$gain, pe.wide$gain, var.equal=T)

cat('\nQ5: No')
leveneTest(data.wide$gain, data.wide$condition)

cat('\nQ6: Yes')
aov.model = aov(data.wide$gain ~ data.wide$condition)
summary(aov.model)

# What is the corresponding eta-squared value? (round to 2 decimal places)
cat('\nQ7: .34')
library(lsr)
etaSquared(aov.model, anova=T)

cat('\nQ8: no')

cat('\nQ9: DS and WM')
TukeyHSD(aov.model)

cat('\nQ10: PE-DS')