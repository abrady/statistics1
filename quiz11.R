# quiz 11
# either fixed (cognitive abilities are innate and cannot be improved) 
# or malleable (cognitive abilities are largely driven by experiences). 
# DVs include verbal, spatial, and intelligence measures,provided before and after training.

setwd("~/ocw/statistics1");
data <- read.table("stats1-datafiles-Stats1.13.HW.11.txt", header = T)
#View(data)

# Null hypothesis: there is no relationship between belief and ability to improve
# use binary logistic regression? 

# Using a t-test, compare verbal scores before and after training in the fixed condition. 
#Is the difference pre-test to post-test significant?
cat('\nQ1: yes')
d.fixed = subset(data,cond=='fixed')
# d.fixed
t.test(d.fixed$verbal.pre, d.fixed$verbal.post, paired=T)

# What are the degrees of freedom for the comparison between pre-test and post-test for the spatial scores?
cat('\nQ2: 49')

# Run a Wilcoxon test for the same comparison (pre-test to post-test on spatial scores, fixed 
# condition). Which of the two tests gives the highest p-value for the comparison?
cat('\nQ3: Wilcox')
wilcox.test(d.fixed$verbal.pre, d.fixed$verbal.post, paired=T)

# What is the effect size (Cohen's d) for the difference between pre-test and post-test spatial
# scores for the malleable condition? (round to two decimal places)
# cat('\nQ4: 0.45') whoops, actually asking for fixed
cat('\nQ4: 0.55')
d.mal = subset(data, cond=='malleable')
d.mal.out = describe(d.mal)
# cohensD(wm.t$post, wm.t$pre, method="paired")
cohensD(d.mal$spatial.post, d.mal$spatial.pre, method="paired")
cohensD(d.fixed$spatial.post, d.fixed$spatial.pre, method="paired")

# Which of the three tasks shows the largest improvements from pre-test to post-test, in the fixed
# condition?
cat('\nQ5: verbal')
t.test(d.fixed$verbal.pre, d.fixed$verbal.post, paired=T)
t.test(d.fixed$spatial.pre, d.fixed$spatial.post, paired=T)
t.test(d.fixed$intel.pre, d.fixed$intel.post, paired=T)

cohensD(d.fixed$verbal.pre, d.fixed$verbal.post, method="paired")
cohensD(d.fixed$spatial.post, d.fixed$spatial.pre, method="paired")
cohensD(d.fixed$intel.pre, d.fixed$intel.post, method="paired")

cat('\nQ6: verbal')
cohensD(d.mal$verbal.pre, d.mal$verbal.post, method="paired")
cohensD(d.mal$spatial.post, d.mal$spatial.pre, method="paired")
cohensD(d.mal$intel.pre, d.mal$intel.post, method="paired")

cat('\nQ7: all')
# wilcox.test(wm$gain ~ wm$train, paired = F)
# wilcox.test(data$spatial.pre, data$verbal.pre, paired=F)
# AND wilcox.test(data$spatial.pre, data$intel.pre, paired=F) 
# AND wilcox.test(data$verbal.pre, data$intel.pre, paired=F)
wilcox.test(data$spatial.pre, data$verbal.pre, paired = F)
wilcox.test(data$spatial.pre, data$intel.pre, paired = F)
wilcox.test(data$verbal.pre, data$intel.pre, paired = F)

cat('\nQ8: malleable')
# pre.m = d$verbal.pre + data.m$spatial.pre + data.m$intel.pre 
# post.m = data.m$verbal.post + data.m$spatial.post + data.m$intel.post 
# cohensD(pre.m, post.m, method="paired")
# pre.f = data.f$verbal.pre + data.f$spatial.pre + data.f$intel.pre 
# post.f = data.f$verbal.post + data.f$spatial.post + data.f$intel.post
# cohensD(pre.f, post.f, method="paired")
#pre.m = data$verbal.pre
#cohensD(d.fixed$verbal.pre, d.fixed$verbal.post, method="pooled")
#cohensD(d.fixed$spatial.post, d.fixed$spatial.pre, method="pooled")
#cohensD(d.fixed$intel.pre, d.fixed$intel.post, method="pooled")
cat('\nQ9: verbal')
cat('\nQ10: depends')